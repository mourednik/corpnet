package corpnet.generator

import corpnet._
import corpnet.network.{Network, NetworkGraph}
import probability_monad.Distribution

import scala.util.{Failure, Success, Try}

case class SocialNetworkParameters(
  p: Double = 0.6,
  density: Int = 8,
  deleteExistingEdges: Boolean = true)

object SocialNetworkGenerator {

  /*
  Tree has height h
  For each node u in ascending level order:
    let level = level of node u
    randomly choose from three cases with geometric distribution:
      case1. same level (most likely)
      case2. same department
      case3. other department (least likely)
    if case1
      let d = int from [1, level] with geometric distribution.
      walk up d
      walk down d (no revisiting)
    else if case2
      let d1 = int from [2, h-level] with geometric distribution.
      walk down d1
    else if case3
      let d1 = int from [1, level] with geometric distribution.
      walk up d1
      let d2 = int from [1, h - level] with geometric distribution
      walk down d1 + d2 (no revisiting)
    end
  */

  def randomDistanceGeometric(p: Double, lower: Int, upper: Int): Int = {
    val geom = Distribution.geometric(p)
    val sample = geom.sample(1).head
    Math.min(lower + sample, upper)
  }

  def orderEdge(edge: Edge): Edge = {
    if (edge._1 < edge._2)
      edge
    else
      (edge._2, edge._1)
  }

  def generateEdges(graph: NetworkGraph, parameters: SocialNetworkParameters): Set[(Node, Node)] = {
    val height = graph.levels.values.max + 1
    val nodesLevels = graph.levels.toSeq.sortBy(_._2)
    val tree = NetworkTreeBuilder.buildTree(graph)
    val edges = for {
      (node, level) ← nodesLevels
      _ ← 1 to parameters.density
      otherNode ← findOtherNode(tree, parameters, height, node)
    } yield {
      val edge = (node, otherNode)
      orderEdge(edge)
    }
    edges.toSet
  }

  def findOtherNode(
    tree: NetworkTree, parameters: SocialNetworkParameters, height: Int, node: Node): Option[Node] =
    randomDistanceGeometric(parameters.p, 1, 2) match {
      case 1 ⇒ findNodeSameLevel(tree, parameters, height, node)
      case 2 ⇒ findNodeSameDepartment(tree, parameters, height, node)
      case 3 ⇒ findNodeOtherDepartment(tree, parameters, height, node)
      case n ⇒ throw new RuntimeException(s"SocialNetworkGenerator.generate(): Unexpected case $n")
    }

  def walkUp(tree: NetworkTree, node: Node, distance: Int): (Node, Node) = {
    var prevVisited: Node = node
    var currentNode: Node = node
    for {
      i ← 1 to distance
      nextNode ← tree.parent(currentNode)
    } {
      prevVisited = currentNode
      currentNode = nextNode
    }
    (currentNode, prevVisited)
  }

  def walkDown(tree: NetworkTree, node: Node, prevVisited: Node, distance: Int): (Option[Node], Int) = {
    var currentNode: Option[Node] = Some(node)
    var actualDistance = 0
    (1 to distance).foreach { i ⇒
      currentNode match {
        case Some(node_) ⇒
          val unfilteredChildren = tree.children(node_)
          val children = unfilteredChildren.filterNot(_ == prevVisited)
          if (children.nonEmpty) {
            actualDistance = i
            currentNode = Some(Distribution.discreteUniform(children).sample(1).head)
          } else
            currentNode = None
        case _ ⇒
      }
    }
    (currentNode, actualDistance)
  }

  def findNodeSameLevel(
    tree: NetworkTree, parameters: SocialNetworkParameters, height: Int, node: Node): Option[Node] = {
    val level = tree.level(node) + 1
    val distanceUp = randomDistanceGeometric(parameters.p, 1, level)
    val (topNode, prevVisited) = walkUp(tree, node, distanceUp)
    val (destination, actualDistance) = walkDown(tree, topNode, prevVisited, distanceUp)
    destination
  }

  def findNodeSameDepartment(
    tree: NetworkTree, parameters: SocialNetworkParameters, height: Int, node: Node): Option[Node] = {
    val distance = randomDistanceGeometric(parameters.p, 2, height - tree.level(node) + 1)
    val (destination, actualDistance) = walkDown(tree, node, node, distance)
    if (actualDistance < 2)
      None
    else
      destination
  }

  def findNodeOtherDepartment(
    tree: NetworkTree, parameters: SocialNetworkParameters, height: Int, node: Node): Option[Node] = {
    val level = tree.level(node) + 1
    val distanceUp = randomDistanceGeometric(parameters.p, 1, level)
    val distanceDown = distanceUp + randomDistanceGeometric(parameters.p, 1, height - level)
    val (topNode, prevVisited) = walkUp(tree, node, distanceUp)
    val (destination, actualDistance) = walkDown(tree, topNode, prevVisited, distanceDown)
    destination
  }

  def insertEdge(network: Network, edge: Edge): (Network, Int) =
    Try {
      Network.insertNonReportingEdge(network, edge._1, edge._2)
    } match {
      case Failure(_) ⇒ (network, 0)
      case Success(v) ⇒ (v, 1)
    }

  def generate(network: Network, params: SocialNetworkParameters): (Network, Int) =
    if (params.density == 0)
      (network, 0)
    else {
      val initialNetwork =
        if (params.deleteExistingEdges)
          Network.deleteAllNonReportingEdges(network)
        else
          network

      val edges = generateEdges(initialNetwork.graph, params)

      edges.foldRight((initialNetwork, 0)) {
        case (edge, (network1, count)) ⇒
          val (network2, inserted) = insertEdge(network1, edge)
          (network2, count + inserted)
      }
    }
}

