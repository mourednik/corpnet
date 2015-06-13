package corpnet.layout

import java.awt.geom.Rectangle2D

import corpnet._

case class TreeLayout(
  positions: Map[Node, Rectangle2D],
  directedEdges: Set[Edge],
  undirectedEdges: Set[Edge]) extends Layout
