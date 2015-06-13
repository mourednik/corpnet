package corpnet.layout

import corpnet.Node
import corpnet.layout.EdgeSelection.EdgeSelection
import corpnet.network.{AdjacencyGraph, NetworkGraph}
import probability_monad._

import scala.collection.parallel.immutable
import scala.collection.parallel.immutable.{ParMap, ParVector}

case class ForceConstants(c1: Double = 2.0, c2: Double = 1.0, c3: Double = 0.1, c4: Double = 0.1)

object ForceConstants {
  def optimizedAllEdges() = ForceConstants(2.0, 1.0, 1.0, 0.1)

  def optimizedDirected() = ForceConstants(2.0, 1.0, 0.2, 0.1)

  def optimizedUndirected() = ForceConstants(2.0, 1.0, 0.2, 0.1)
}

case class ForceLayoutParameters(
  constants: ForceConstants = ForceConstants.optimizedAllEdges(),
  iterations: Int = 100,
  edgeSelection: EdgeSelection = EdgeSelection.All)

object EdgeSelection extends Enumeration {
  type EdgeSelection = Value
  val All, Directed, Undirected = Value
}

case class Position(x: Double, y: Double) {

  def *(force: Force, factor: Double): Position = {
    val x2 = x + force.xUnit * force.magnitude * factor
    val y2 = y + force.yUnit * force.magnitude * factor
    Position(x2, y2)
  }

  def *(scale: Double): Position =
    Position(x * scale, y * scale)
}

object ForceLayoutBuilder {

  val uniformDist = Distribution.uniform

  def sumForces(
    node: Node,
    otherNodes: ParVector[Node],
    adjacentNodes: Set[Node],
    positions: ParMap[Node, Position])(implicit params: ForceConstants): Force = {

    val forces =
      otherNodes.map { otherNode ⇒
        if (adjacentNodes.contains(otherNode))
          Force.adjacent(positions(otherNode), positions(node))
        else
          Force.nonAdjacent(positions(otherNode), positions(node))
      }
    forces.reduce((x: Force, y: Force) ⇒ x + y)
  }

  def randomPositionsNew(nodes: List[Node])(implicit params: ForceConstants): ParMap[Node, Position] = {
    val scale = Math.sqrt(nodes.size)
    (nodes zip (uniformDist.sample(nodes.size) zip uniformDist.sample(nodes.size)) map {
      case (node, (x, y)) ⇒
        node -> Position(scale * x, scale * y)
    }).toMap.par
  }

  def randomPositionsFromPrevious(nodes: List[Node], prevPositions: immutable.ParMap[Node, Position])(implicit params: ForceConstants): ParMap[Node, Position] = {
    val scale = Math.sqrt(nodes.size)
    val positions = for {
      node ← nodes
    } yield {
      prevPositions.get(node) match {
        case Some(position) ⇒
          node -> position
        case None ⇒
          val x = uniformDist.sample(1).head
          val y = uniformDist.sample(1).head
          node -> Position(scale * x, scale * y)
      }
    }
    val result = positions.toMap.par
    result
  }

  def randomPositions(nodes: List[Node], prevLayout: Option[ForceLayout])(implicit params: ForceConstants): ParMap[Node, Position] = {
    prevLayout match {
      case Some(layout) ⇒ randomPositionsFromPrevious(nodes, scaleDownPositions(layout.positions))
      case None         ⇒ randomPositionsNew(nodes)
    }
  }

  def scaleUpPositions(positions: immutable.ParMap[Node, Position]): immutable.ParMap[Node, Position] =
    positions.mapValues(_ * 50.0).toMap

  def scaleDownPositions(positions: immutable.ParMap[Node, Position]): immutable.ParMap[Node, Position] =
    positions.mapValues(_ * (1 / 50.0)).toMap

  def build(graph: NetworkGraph, params: ForceLayoutParameters, prevLayout: Option[ForceLayout]): ForceLayout = {
    implicit val forceConstants = params.constants
    val usingPrevPositions = prevLayout.isDefined

    val adjacencyGraph = AdjacencyGraph(graph)
    var positions: immutable.ParMap[Node, Position] = randomPositions(adjacencyGraph.nodes, prevLayout)

    val nodes = adjacencyGraph.nodes.toVector.par

    val adjacent = params.edgeSelection match {
      case EdgeSelection.All        ⇒ adjacencyGraph.adjacent
      case EdgeSelection.Directed   ⇒ adjacencyGraph.adjacentDirected
      case EdgeSelection.Undirected ⇒ adjacencyGraph.adjacentUndirected
    }

    for (i ← 1 to params.iterations) {
      val factor = if (usingPrevPositions) params.constants.c4 else params.constants.c4 + Math.pow(0.95, i)
      positions = positions.map {
        case (node, position) ⇒
          val force = sumForces(node, nodes.filterNot(_ == node), adjacent(node), positions)
          val nextPosition = position * (force, factor)
          node -> nextPosition
      }
    }

    lazy val undirectedEdges = NetworkGraph.findDistinctUndirectedEdges(graph) ++ NetworkGraph.findTopManagerEdges(graph)
    lazy val directedEdges = NetworkGraph.findSubordinateEdges(graph)

    val scaledPositions = scaleUpPositions(positions)

    params.edgeSelection match {
      case EdgeSelection.All ⇒
        ForceLayout(scaledPositions, directedEdges, undirectedEdges, graph.topManagers)
      case EdgeSelection.Directed ⇒
        ForceLayout(scaledPositions, directedEdges, Set.empty, graph.topManagers)
      case EdgeSelection.Undirected ⇒
        ForceLayout(scaledPositions, Set.empty, undirectedEdges, graph.topManagers)
    }
  }
}
