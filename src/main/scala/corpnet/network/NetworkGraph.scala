package corpnet.network

import breeze.linalg._
import corpnet._

import scala.collection.{ immutable, mutable }
import scala.util.Try

case class NetworkGraph(
  nodes: immutable.Vector[Node],
  levels: NodeLevel,
  topManagers: Set[Node],
  numCoworkers: Map[Node, Int],
  numSubordinates: Map[Node, Int],
  managerMatrix: AdjacencyMatrix,
  subordinateMatrix: AdjacencyMatrix,
  coworkerMatrix: AdjacencyMatrix,
  topManagerMatrix: AdjacencyMatrix,
  nextNodeId: Node = 0)

object NetworkGraph {

  def apply(): NetworkGraph = {
    new NetworkGraph(
      immutable.Vector.empty,
      Map.empty,
      Set.empty,
      Map.empty,
      Map.empty,
      DenseMatrix.zeros[Double](0, 0),
      DenseMatrix.zeros[Double](0, 0),
      DenseMatrix.zeros[Double](0, 0),
      DenseMatrix.zeros[Double](0, 0))
  }

  // Matrix

  private def expandAdjacencyMatrix(matrix: AdjacencyMatrix, num: Int = 1): AdjacencyMatrix = {
    val newSize = matrix.rows + num
    val newMatrix = DenseMatrix.zeros[Double](newSize, newSize)
    if (matrix.rows > 0)
      newMatrix(0 until matrix.rows, 0 until matrix.rows) := matrix
    newMatrix
  }

  private def reduceAdjacencyMatrix(matrix: AdjacencyMatrix, excludedIndices: Set[Int]): AdjacencyMatrix = {
    val newSize = matrix.rows - excludedIndices.size
    val newMatrix = DenseMatrix.zeros[Double](newSize, newSize)
    var i2 = -1
    for (i1 ← 0 until matrix.rows if !excludedIndices.contains(i1)) {
      i2 += 1
      var j2 = -1
      for (j1 ← 0 until matrix.rows if !excludedIndices.contains(j1)) {
        j2 += 1
        newMatrix(i2, j2) = matrix(i1, j1)
      }
    }
    newMatrix
  }

  // Nodes

  def prepareForBulkInsert(graph: NetworkGraph, n: Int): NetworkGraph = {
    val managerMatrix = expandAdjacencyMatrix(graph.managerMatrix, n)
    val subordinateMatrix = expandAdjacencyMatrix(graph.subordinateMatrix, n)
    val coworkerMatrix = expandAdjacencyMatrix(graph.coworkerMatrix, n)
    val topManagerMatrix = expandAdjacencyMatrix(graph.topManagerMatrix, n)
    graph.copy(managerMatrix = managerMatrix, subordinateMatrix = subordinateMatrix, coworkerMatrix = coworkerMatrix,
      topManagerMatrix = topManagerMatrix)
  }

  private def prepareForSingleInsert(graph: NetworkGraph) = {
    val nodeId = graph.nextNodeId
    val nodes = graph.nodes :+ nodeId
    val managerMatrix = expandAdjacencyMatrix(graph.managerMatrix)
    val subordinateMatrix = expandAdjacencyMatrix(graph.subordinateMatrix)
    val coworkerMatrix = expandAdjacencyMatrix(graph.coworkerMatrix)
    val topManagerMatrix = expandAdjacencyMatrix(graph.topManagerMatrix)
    val numSubordinates = graph.numSubordinates + (nodeId -> 0)
    val numCoworkers = graph.numCoworkers + (nodeId -> 0)
    (nodeId, nodes, numCoworkers, numSubordinates, managerMatrix, subordinateMatrix, coworkerMatrix, topManagerMatrix)
  }

  def insertTopManager(graph: NetworkGraph): (NetworkGraph, Node) = {
    val (nodeId, nodes, numCoworkers, numSubordinates, managerMatrix,
      subordinateMatrix, coworkerMatrix, topManagerMatrix) = prepareForSingleInsert(graph)
    val topManagers = graph.topManagers + nodeId
    val levels = graph.levels + (nodeId -> 0)
    val newGraph = NetworkGraph(
      nodes, levels, topManagers, numCoworkers, numSubordinates,
      managerMatrix, subordinateMatrix, coworkerMatrix, topManagerMatrix, nodeId + 1)
    (connectTopManagers(newGraph), nodeId)
  }

  def insertSubordinate(graph: NetworkGraph, manager: Node): (NetworkGraph, Node) = {
    val (nodeId, nodes, numCoworkers, numSubordinates, managerMatrix,
      subordinateMatrix, coworkerMatrix, topManagerMatrix) = prepareForSingleInsert(graph)
    val managerLevel = findLevel(graph, manager)
    val levels = graph.levels + (nodeId -> (managerLevel + 1))
    val numSubordinates2 = numSubordinates + (manager -> (numSubordinates(manager) + 1))
    val newGraph = NetworkGraph(
      nodes, levels, graph.topManagers, numCoworkers, numSubordinates2,
      managerMatrix, subordinateMatrix, coworkerMatrix, topManagerMatrix, nodeId + 1)
    (insertSubordinateEdge(newGraph, manager, nodeId), nodeId)
  }

  def bulkInsertTopManager(graph: NetworkGraph): (NetworkGraph, Node) = {
    val nodeId = graph.nextNodeId
    val nodes = graph.nodes :+ nodeId
    val topManagers = graph.topManagers + nodeId
    val levels = graph.levels + (nodeId -> 0)
    val newGraph = graph.copy(nodes = nodes, levels = levels, topManagers = topManagers, nextNodeId = nodeId + 1)
    (connectTopManagers(finishBulkInsert(newGraph)), nodeId)
  }

  def bulkInsertSubordinate(graph: NetworkGraph, node: Node): (NetworkGraph, Node) = {
    val subordinate = graph.nextNodeId
    val nodes = graph.nodes :+ subordinate
    val managerLevel = findLevel(graph, node)
    val levels = graph.levels + (subordinate -> (managerLevel + 1))
    val graph2 = graph.copy(nodes = nodes, levels = levels, nextNodeId = subordinate + 1)
    val graph3 = insertSubordinateEdge(graph2, node, subordinate)
    (finishBulkInsert(graph3), subordinate)
  }

  private def numCoworkersVector(graph: NetworkGraph): immutable.Vector[Int] = {
    val ones = DenseVector.ones[Double](graph.nodes.size)
    (graph.coworkerMatrix * ones).toScalaVector.map(Math.round).map(_.toInt)
  }

  private def numSubordinatesVector(graph: NetworkGraph): immutable.Vector[Int] = {
    val ones = DenseVector.ones[Double](graph.nodes.size)
    (graph.subordinateMatrix * ones).toScalaVector.map(Math.round).map(_.toInt)
  }

  private def finishBulkInsert(graph: NetworkGraph): NetworkGraph =
    if (graph.coworkerMatrix.rows == graph.nodes.size) {
      val ones = DenseVector.ones[Double](graph.nodes.size)
      val numCoworkersVec = numCoworkersVector(graph)
      val numSubordinatesVec = numSubordinatesVector(graph)
      val numCoworkersMap = (graph.nodes zip numCoworkersVec).map(x ⇒ x._1 -> x._2).toMap
      val numSubordinatesMap = (graph.nodes zip numSubordinatesVec).map(x ⇒ x._1 -> x._2).toMap
      graph.copy(numCoworkers = numCoworkersMap, numSubordinates = numSubordinatesMap)
    } else {
      graph
    }

  private def connectTopManagers(graph: NetworkGraph): NetworkGraph = {
    val n = graph.topManagerMatrix.rows
    val topManagerMatrix = DenseMatrix.zeros[Double](n, n)
    graph.topManagers.size match {
      case 1 ⇒
        val topManagerIndex = findIndex(graph, graph.topManagers.head)
        topManagerMatrix(topManagerIndex, topManagerIndex) = 1.0
      case _ ⇒
        for {
          manager1 ← graph.topManagers
          manager2 ← graph.topManagers
          if manager1 != manager2
        } {
          val manager1Index = findIndex(graph, manager1)
          val manager2Index = findIndex(graph, manager2)
          topManagerMatrix(manager1Index, manager2Index) = 1.0
        }
    }
    graph.copy(topManagerMatrix = topManagerMatrix)
  }

  def findChildren(graph: NetworkGraph, node: Node): Set[Node] = {
    val nodeIndex = findIndex(graph, node)
    val row = graph.subordinateMatrix(nodeIndex, ::)
    val nonZero = row.inner :> 0.0
    nonZero.activeKeysIterator.toSet.map(graph.nodes)
  }

  def findParent(graph: NetworkGraph, node: Node): Option[Node] = {
    if (graph.topManagers.contains(node))
      None
    else {
      val nodeIndex = findIndex(graph, node)
      val col = graph.subordinateMatrix(::, nodeIndex)
      val tmp = col :> 0.0
      tmp.activeKeysIterator.toIterable.headOption.map(graph.nodes)
    }
  }

  private def findSubTree(graph: NetworkGraph, node: Node): Set[Node] = {
    val subTree = mutable.Set(node)
    val queue = mutable.Queue(node)
    while (queue.nonEmpty) {
      val nextNode = queue.dequeue()
      val children = findChildren(graph, nextNode)
      subTree ++= children
      queue ++= children
    }
    subTree.toSet
  }

  def deleteNode(graph: NetworkGraph, node: Node): NetworkGraph = {
    val subtreeNodes = findSubTree(graph, node)
    val subtreeNodeIndices = subtreeNodes.map(graph.nodes.indexOf)
    val remainingNodes = graph.nodes.filterNot(subtreeNodes.contains)
    val isTopManager = graph.topManagers.contains(node)
    val topManagers = if (isTopManager) graph.topManagers.filterNot(_ == node) else graph.topManagers
    val managerMatrix = reduceAdjacencyMatrix(graph.managerMatrix, subtreeNodeIndices)
    val subordinateMatrix = reduceAdjacencyMatrix(graph.subordinateMatrix, subtreeNodeIndices)
    val coworkerMatrix = reduceAdjacencyMatrix(graph.coworkerMatrix, subtreeNodeIndices)
    val topManagerMatrix = reduceAdjacencyMatrix(graph.topManagerMatrix, subtreeNodeIndices)
    val levels = graph.levels.filterKeys(nodeId ⇒ !subtreeNodes.contains(nodeId))

    val coworkers = findCoworkers(graph, node)
    val manager = findManager(graph, node)
    val reducedNumCoworkers = graph.numCoworkers.filterKeys(coworkers.contains).mapValues(_ - 1)
    val reducedNumSubordinates =
      if (node != manager)
        graph.numSubordinates + (manager -> (graph.numSubordinates(manager) - 1))
      else
        Map.empty
    val numCoworkers = graph.numCoworkers
      .filterKeys(nodeId ⇒ nodeId != node && !subtreeNodes.contains(nodeId)) ++ reducedNumCoworkers
    val numSubordinates = graph.numSubordinates
      .filterKeys(nodeId ⇒ nodeId != node && !subtreeNodes.contains(nodeId)) ++ reducedNumSubordinates

    val newGraph = NetworkGraph(remainingNodes, levels, topManagers, numCoworkers, numSubordinates,
      managerMatrix, subordinateMatrix, coworkerMatrix, topManagerMatrix, graph.nextNodeId)
    if (isTopManager)
      connectTopManagers(newGraph)
    else
      newGraph
  }

  // Edges

  private def insertSubordinateEdge(graph: NetworkGraph, manager: Node, subordinate: Node): NetworkGraph = {
    val managerIndex = findIndex(graph, manager)
    val subordinateIndex = findIndex(graph, subordinate)
    graph.subordinateMatrix(managerIndex, subordinateIndex) = 1.0
    graph.managerMatrix(subordinateIndex, managerIndex) = 1.0
    graph
  }

  def insertUndirectedEdge(graph: NetworkGraph, node1: Node, node2: Node): NetworkGraph = {
    updateUndirectedEdge(graph, node1, node2, connect = true)
  }

  def deleteUndirectedEdge(graph: NetworkGraph, node1: Node, node2: Node): NetworkGraph = {
    updateUndirectedEdge(graph, node1, node2, connect = false)
  }

  def deleteAllUndirectedEdges(graph: NetworkGraph): NetworkGraph = {
    val n = graph.nodes.size
    val zeroMatrix = DenseMatrix.zeros[Double](n, n)
    val numCoworkers = graph.numCoworkers.mapValues(_ ⇒ 0)
    graph.copy(coworkerMatrix = zeroMatrix, numCoworkers = numCoworkers)
  }

  private def updateUndirectedEdge(graph: NetworkGraph, node1: Node, node2: Node, connect: Boolean): NetworkGraph = {
    if (graph.topManagers.contains(node1) && graph.topManagers.contains(node2))
      throw new IllegalArgumentException("Can not modify undirected edges between top managers")

    val node1Manager = findManager(graph, node1)
    val node2Manager = findManager(graph, node2)
    if (node2Manager == node1 || node1Manager == node2)
      throw new IllegalArgumentException("Can not insert undirected edge between a manager and its subordinate")

    val node1Index = findIndex(graph, node1)
    val node2Index = findIndex(graph, node2)

    val increment = if (connect) 1 else -1
    val numCoworkers = graph.numCoworkers ++ Map(
      node1 -> (graph.numCoworkers(node1) + increment),
      node2 -> (graph.numCoworkers(node2) + increment))

    val value = if (connect) 1.0 else 0.0
    val coworkerMatrix = graph.coworkerMatrix.copy
    coworkerMatrix(node1Index, node2Index) = value
    coworkerMatrix(node2Index, node1Index) = value
    graph.copy(coworkerMatrix = coworkerMatrix, numCoworkers = numCoworkers)
  }

  // info

  def findIndex(graph: NetworkGraph, node: Node): Int =
    graph.nodes.indexOf(node)

  def findManager(graph: NetworkGraph, node: Node): Node = {
    if (graph.topManagers.contains(node))
      return node
    val nodeIndex = findIndex(graph, node)
    val managerVector = graph.managerMatrix(nodeIndex, ::).inner
    for (i ← 0 until managerVector.iterableSize)
      if (managerVector(i) == 1.0)
        return graph.nodes(i)
    throw new RuntimeException(s"Could not find manager for node [$node]")
  }

  def findCoworkers(graph: NetworkGraph, node: Node): List[Node] =
    findNodesAdjacentVia(graph, node, (graph: NetworkGraph) ⇒ graph.coworkerMatrix)

  def findSubordinates(graph: NetworkGraph, node: Node): List[Node] =
    findNodesAdjacentVia(graph, node, (graph: NetworkGraph) ⇒ graph.subordinateMatrix)

  def findLevel(graph: NetworkGraph, node: Node): Int = {
    var level = 0
    var currentNode = node
    while (!graph.topManagers.contains(currentNode)) {
      currentNode = findManager(graph, currentNode)
      level += 1
    }
    level
  }

  def findNumCoworkers(graph: NetworkGraph, node: Node): Int = {
    val nodeIndex = findIndex(graph, node)
    val nonReportingRelations = graph.coworkerMatrix
    val vector = nonReportingRelations(nodeIndex, ::).inner
    math.round(sum(vector)).toInt
  }

  def findNumSubordinates(graph: NetworkGraph, node: Node): Int = {
    val nodeIndex = findIndex(graph, node)
    val vector = graph.subordinateMatrix(nodeIndex, ::).inner
    math.round(sum(vector)).toInt
  }

  def findNodesAdjacentVia(graph: NetworkGraph, node: Node, relation: NetworkGraph ⇒ AdjacencyMatrix): List[Node] = {
    val nodeIndex = findIndex(graph, node)
    val adjacencyVector = relation(graph)(nodeIndex, ::).inner
    adjacencyVector.activeIterator
      .filter(kv ⇒ kv._2 == 1.0)
      .map(kv ⇒ graph.nodes(kv._1))
      .toList
  }

  def findTopManagerEdges(graph: NetworkGraph): Set[Edge] = {
    graph.topManagers.size match {
      case 0 ⇒ Set.empty
      case 1 ⇒ Set((graph.topManagers.head, graph.topManagers.head))
      case _ ⇒
        val edges = for {
          t1 ← graph.topManagers
          t2 ← graph.topManagers if t1 != t2
        } yield {
          (t1, t2)
        }
        distinctUndirectedEdges(edges)
    }
  }

  def findSubordinateEdges(graph: NetworkGraph): Set[Edge] =
    for {
      node ← graph.nodes.toSet[Node]
      subordinate ← findSubordinates(graph, node)
    } yield {
      (node, subordinate)
    }

  def findBackflowEdges(graph: NetworkGraph): Set[Edge] =
    for {
      node ← graph.nodes.toSet[Node]
      manager ← Try(findManager(graph, node)).toOption
      if node != manager
    } yield {
      (node, manager)
    }

  def findUndirectedEdges(graph: NetworkGraph): Set[Edge] =
    for {
      node ← graph.nodes.toSet[Node]
      coworker ← findCoworkers(graph, node)
    } yield {
      (node, coworker)
    }

  def findDistinctUndirectedEdges(graph: NetworkGraph): Set[Edge] = {
    val edges = for {
      node ← graph.nodes.toList
      coworker ← findCoworkers(graph, node)
    } yield {
      (node, coworker)
    }
    distinctUndirectedEdges(edges)
  }

  private def distinctUndirectedEdges(edges: Iterable[Edge]): Set[Edge] = {
    def reverse(edge: Edge) = (edge._2, edge._1)
    val distinctEdges = mutable.Set.empty[Edge]
    edges.foreach { edge ⇒
      if (!distinctEdges.contains(reverse(edge)))
        distinctEdges += edge
    }
    distinctEdges.toSet
  }

  def hasUndirectedEdge(graph: NetworkGraph, node1: Node, node2: Node) = {
    val node1Index = findIndex(graph, node1)
    val node2Index = findIndex(graph, node2)
    graph.coworkerMatrix(node1Index, node2Index) == 1.0
  }

  def findNodesAdjacentAll(graph: NetworkGraph, node: Node): Set[Node] = {
    val subordinates = NetworkGraph.findSubordinates(graph, node).toSet
    val coworkers = NetworkGraph.findCoworkers(graph, node)
    if (graph.topManagers.contains(node)) {
      subordinates ++ coworkers ++ graph.topManagers.filterNot(_ == node)
    } else {
      subordinates ++ coworkers + NetworkGraph.findManager(graph, node)
    }
  }

  def findNodesAdjacentDirected(graph: NetworkGraph, node: Node): Set[Node] = {
    val subordinates = NetworkGraph.findSubordinates(graph, node).toSet
    if (graph.topManagers.contains(node)) {
      subordinates
    } else {
      subordinates + NetworkGraph.findManager(graph, node)
    }
  }

  def findNodesAdjacentUndirected(graph: NetworkGraph, node: Node): Set[Node] = {
    val coworkers = NetworkGraph.findCoworkers(graph, node).toSet
    if (graph.topManagers.contains(node)) {
      coworkers ++ graph.topManagers.filterNot(_ == node)
    } else
      coworkers
  }

  def findAllManagers(graph: NetworkGraph): Set[Node] = {
    val middleManagers = for {
      (nodeId, numSubordinates) ← graph.nodes zip numSubordinatesVector(graph)
      if numSubordinates > 0
    } yield {
      nodeId
    }
    graph.topManagers ++ middleManagers
  }
}
