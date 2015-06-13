package corpnet.network

import corpnet._

import scala.util._

case class WeightedEdge(edge: Edge, weight: Double)

case class Network(parameters: NetworkParameters, graph: NetworkGraph)

object Network {

  val networkProtocol = {
    import scala.pickling._
    import scala.pickling.Defaults._
    import scala.pickling.json._

    new pickler.PrimitivePicklers with pickler.RefPicklers with json.JsonFormats {
      implicit val networkPickler = PicklerUnpickler.generate[Network]
      implicit val so = static.StaticOnly

      def toJsonString[Network: Pickler](network: Network): String =
        scala.pickling.functions.pickle(network).value

      def fromJsonString[Network: Unpickler](string: String): Network =
        scala.pickling.functions.unpickle[Network](json.JSONPickle(string))
    }
  }

  def apply(parameters: NetworkParameters = NetworkParameters()): Network = {
    checkAllParameters(parameters)
    new Network(parameters, NetworkGraph())
  }

  // Parameters

  def checkAllParameters(parameters: NetworkParameters, network: Option[Network] = None): Unit = {
    checkBetaMultiplier(parameters.betaMultiplier)
    checkCapacityParameters(parameters.delta, parameters.w, parameters.s)
    checkK(parameters.k)
    checkT(parameters.t)
    checkMaxDepth(parameters.maxDepth, network.map(_.graph.levels))
  }

  def checkCapacityParameters(delta: Double, w: Double, s: Double): Unit = {
    if (delta <= 0.0)
      throw new IllegalArgumentException("delta <= 0.0")
    if (w <= 0.0)
      throw new IllegalArgumentException("w <= 0.0")
    if (s <= 0.0)
      throw new IllegalArgumentException("s <= 0.0")
    if (delta < s)
      throw new IllegalArgumentException("delta < s")
  }

  def checkK(k: Double): Unit = {
    if (k <= 0.0)
      throw new IllegalArgumentException("k <= 0.0")
    if (k > 1.0)
      throw new IllegalArgumentException("k > 1.0")
  }

  def checkT(t: Double): Unit = {
    if (t < 0.0)
      throw new IllegalArgumentException("t < 0.0")
  }

  def checkBetaMultiplier(betaMultiplier: Double): Unit = {
    if (betaMultiplier <= 0.0)
      throw new IllegalArgumentException("betaMultiplier <= 0.0")
    if (betaMultiplier >= 1.0)
      throw new IllegalArgumentException("betaMultiplier >= 0.0")
  }

  def checkMaxDepth(maxDepth: Int, levels: Option[NodeLevel]): Unit =
    if (maxDepth < 1)
      throw new IllegalArgumentException("maxDepth must be at least 1")
    else levels match {
      case Some(levels_) if levels_.values.nonEmpty && maxDepth < (levels_.values.max + 1) ⇒
        throw new IllegalArgumentException("new maxDepth value is less than current tree depth")
      case _ ⇒ // ignore
    }

  def updateParameters(oldNetwork: Network, newParameters: NetworkParameters): Network = {
    checkAllParameters(newParameters, Some(oldNetwork))
    val network = oldNetwork.copy(parameters = newParameters)
    val usedCapacities = network.graph.nodes.map {
      node ⇒ calculateNodeUsedCapacity(network, node)
    }
    usedCapacities.count(_ > newParameters.delta) match {
      case 0 ⇒
        network
      case nonZero ⇒
        throw new RuntimeException(s"Capacity would be exceeded for $nonZero nodes. Try increasing delta.")
    }
  }

  def tryUpdateParameters(oldNetwork: Network, newParameters: NetworkParameters): Option[Network] =
    Try {
      updateParameters(oldNetwork, newParameters)
    } match {
      case Success(v) ⇒ Some(v)
      case Failure(_) ⇒ None
    }

  // graph updates

  def insertTopManager(network: Network): (Network, Node) = {
    val (graph, topManager) = NetworkGraph.insertTopManager(network.graph)
    (network.copy(graph = graph), topManager)
  }

  def insertSubordinate(network: Network, node: Node): (Network, Node) = {
    val managerLevel = NetworkGraph.findLevel(network.graph, node)
    if (managerLevel == (network.parameters.maxDepth - 1))
      throw new RuntimeException("Failed to insert subordinate; it would exceed max tree depth")

    if (!checkNodeReportingCapacityAvailable(network, node))
      throw new RuntimeException("Failed to insert subordinate; it would exceed managers capacity")

    val (graph, subordinate) = NetworkGraph.insertSubordinate(network.graph, node)
    (network.copy(graph = graph), subordinate)
  }

  def prepareForBulkInsert(network: Network, n: Int): Network = {
    val graph = NetworkGraph.prepareForBulkInsert(network.graph, n)
    network.copy(graph = graph)
  }

  def bulkInsertSubordinate(network: Network, node: Node): (Network, Node) = {
    val (graph, subordinate) = NetworkGraph.bulkInsertSubordinate(network.graph, node)
    (network.copy(graph = graph), subordinate)
  }

  def deleteNode(network: Network, node: Node): Network = {
    val graph = NetworkGraph.deleteNode(network.graph, node)
    network.copy(graph = graph)
  }

  def toggleNonReportingEdge(network: Network, node1: Node, node2: Node): Network = {
    val edgeExists = NetworkGraph.hasUndirectedEdge(network.graph, node1, node2)
    if (edgeExists)
      deleteNonReportingEdge(network, node1, node2)
    else
      insertNonReportingEdge(network, node1, node2)
  }

  def insertNonReportingEdge(network: Network, node1: Node, node2: Node): Network = {
    if (!checkNodeNonReportingCapacityAvailable(network, node1))
      throw new RuntimeException("Failed to insert coworker edge; it would exceed node1 capacity")
    if (!checkNodeNonReportingCapacityAvailable(network, node2))
      throw new RuntimeException("Failed to insert coworker edge; it would exceed node2 capacity")

    val graph = NetworkGraph.insertUndirectedEdge(network.graph, node1, node2)
    network.copy(graph = graph)
  }

  def deleteNonReportingEdge(network: Network, node1: Node, node2: Node): Network = {
    val graph = NetworkGraph.deleteUndirectedEdge(network.graph, node1, node2)
    network.copy(graph = graph)
  }

  def deleteAllNonReportingEdges(network: Network): Network = {
    val graph = NetworkGraph.deleteAllUndirectedEdges(network.graph)
    network.copy(graph = graph)
  }

  // Capacity

  def calculateNodeUsedCapacity(network: Network, node: Node): Double = {
    val numCoworkers = network.graph.numCoworkers(node)
    val numSubordinates = network.graph.numSubordinates(node)
    network.parameters.w * numCoworkers + network.parameters.s * (1 + numSubordinates)
  }

  def checkNodeReportingCapacityAvailable(network: Network, node: Node): Boolean = {
    val usedCapacity = calculateNodeUsedCapacity(network, node)
    network.parameters.delta >= usedCapacity + network.parameters.s
  }

  def checkNodeNonReportingCapacityAvailable(network: Network, node: Node): Boolean = {
    val usedCapacity = calculateNodeUsedCapacity(network, node)
    network.parameters.delta >= usedCapacity + network.parameters.w
  }

  // Edges

  def findTopManagerWeightedEdges(network: Network): Set[WeightedEdge] = {
    val edges = NetworkGraph.findTopManagerEdges(network.graph)
    edges.map(edge ⇒ WeightedEdge(edge, network.parameters.t))
  }

  def findBackflowWeightedEdges(network: Network): Set[WeightedEdge] = {
    val edges = NetworkGraph.findBackflowEdges(network.graph)
    edges.map(edge ⇒ WeightedEdge(edge, network.parameters.k))
  }

  def findSubordinateWeightedEdges(network: Network): Set[WeightedEdge] = {
    val edges = NetworkGraph.findSubordinateEdges(network.graph)
    edges.map(edge ⇒ WeightedEdge(edge, 1.0))
  }

  def findUndirectedWeightedEdges(network: Network): Set[WeightedEdge] = {
    val edges = NetworkGraph.findUndirectedEdges(network.graph)
    edges.map(edge ⇒ WeightedEdge(edge, network.parameters.w))
  }

  def findWeightedEdges(network: Network,
    topManagers: Boolean, subordinates: Boolean, backflow: Boolean, undirected: Boolean): Set[WeightedEdge] = {
    val topManagerEdges = topManagers match {
      case true  ⇒ Network.findTopManagerWeightedEdges(network)
      case false ⇒ Set.empty[WeightedEdge]
    }
    val subordinateEdges = subordinates match {
      case true  ⇒ Network.findSubordinateWeightedEdges(network)
      case false ⇒ Set.empty[WeightedEdge]
    }
    val backflowEdges = backflow match {
      case true  ⇒ Network.findBackflowWeightedEdges(network)
      case false ⇒ Set.empty[WeightedEdge]
    }
    val undirectedEdges = undirected match {
      case true  ⇒ Network.findUndirectedWeightedEdges(network)
      case false ⇒ Set.empty[WeightedEdge]
    }
    topManagerEdges ++ subordinateEdges ++ backflowEdges ++ undirectedEdges
  }

}
