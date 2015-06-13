package corpnet.layout

import corpnet._

import scala.collection.parallel.immutable.ParMap

case class ForceLayout(
  positions: ParMap[Node, Position],
  directedEdges: Set[Edge],
  undirectedEdges: Set[Edge],
  topManagers: Set[Node],
  nodeSize: Double = 15.0) extends Layout
