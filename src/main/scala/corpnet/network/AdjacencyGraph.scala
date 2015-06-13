package corpnet.network

import corpnet._

case class AdjacencyGraph(
  nodes: List[Node],
  adjacent: Map[Node, Set[Node]],
  adjacentDirected: Map[Node, Set[Node]],
  adjacentUndirected: Map[Node, Set[Node]])

object AdjacencyGraph {
  def apply(graph: NetworkGraph): AdjacencyGraph = {
    val adjacent = graph.nodes.map(node ⇒ node -> NetworkGraph.findNodesAdjacentAll(graph, node)).toMap
    val adjacentDirected = graph.nodes.map(node ⇒ node -> NetworkGraph.findNodesAdjacentDirected(graph, node)).toMap
    val adjacentUndirected = graph.nodes.map(node ⇒ node -> NetworkGraph.findNodesAdjacentUndirected(graph, node)).toMap
    AdjacencyGraph(graph.nodes.toList, adjacent, adjacentDirected, adjacentUndirected)
  }
}
