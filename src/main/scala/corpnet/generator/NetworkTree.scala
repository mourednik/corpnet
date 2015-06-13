package corpnet.generator

import corpnet._

import scala.annotation.tailrec

case class NetworkTree(
  children: Map[Node, Set[Node]],
  parent: Map[Node, Option[Node]],
  level: NodeLevel,
  height: Int)

object NetworkTree {
  val ghostRootID = -1

  def leafNodes(tree: NetworkTree): List[Node] =
    tree.children.filter(_._2.isEmpty).keySet.toList

  def chain(node: Node, tree: NetworkTree): List[Node] = {
    @tailrec
    def go(nodes: List[Node]): List[Node] =
      tree.parent(nodes.head) match {
        case Some(parent) if parent != ghostRootID ⇒ go(parent :: nodes)
        case _                                     ⇒ nodes
      }
    go(List(node))
  }
}
