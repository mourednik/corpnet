package corpnet.generator

import corpnet.network.NetworkGraph

object NetworkTreeBuilder {

  def buildTree(graph: NetworkGraph): NetworkTree = {
    val ghostRootChildren = Map(
      NetworkTree.ghostRootID -> graph.topManagers)
    val children = graph.nodes.map {
      node ⇒ node -> NetworkGraph.findChildren(graph, node)
    }.toMap
    val parent = graph.nodes.map {
      node ⇒ node -> NetworkGraph.findParent(graph, node)
    }.toMap
    val topManagerParents = graph.topManagers.map {
      manager ⇒ manager -> Some(NetworkTree.ghostRootID)
    }.toMap
    val height = graph.levels.values.max + 1
    NetworkTree(ghostRootChildren ++ children, parent ++ topManagerParents, graph.levels, height)
  }
}
