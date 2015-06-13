package corpnet.layout

import java.awt.geom.Rectangle2D

import corpnet._
import corpnet.network.{Network, NetworkGraph}
import org.abego.treelayout.util.{DefaultConfiguration, DefaultTreeForTreeLayout}
import org.abego.treelayout.{TreeLayout ⇒ TreeLayoutGenerator}

import scala.collection.JavaConversions._
import scala.collection.mutable

case class LayoutNode(index: Node)

case class TreeLayoutParameters(
  nodeWidth: Double = 40.0,
  nodeHeight: Double = 20.0,
  levelGap: Double = 20.0,
  nodeGap: Double = 20.0)

object TreeLayoutBuilder {

  private def buildDefaultTreeForTreeLayout(graph: NetworkGraph): DefaultTreeForTreeLayout[LayoutNode] = {
    // Ghost root node is parent of top managers. Required in order to create treelayout but not displayed in GUI
    val ghostRoot = LayoutNode(-1)
    val tree = new DefaultTreeForTreeLayout[LayoutNode](ghostRoot)

    val children = graph.topManagers.map(LayoutNode)
    children.foreach(child ⇒ tree.addChild(ghostRoot, child))

    val queue = mutable.Queue[LayoutNode]()
    queue ++= children

    while (queue.nonEmpty) {
      val currentNode = queue.dequeue()
      val children = NetworkGraph.findChildren(graph, currentNode.index).map(LayoutNode)
      children.foreach(child ⇒ tree.addChild(currentNode, child))
      queue ++= children
    }
    tree
  }

  def buildTreeLayout(network: Network, params: TreeLayoutParameters): TreeLayout = {

    val nodeExtentProvider = new NetworkNodeExtentProvider(params.nodeWidth, params.nodeHeight)
    val configuration = new DefaultConfiguration[LayoutNode](params.levelGap, params.nodeGap)
    val tree = buildDefaultTreeForTreeLayout(network.graph)

    val treeLayout = new TreeLayoutGenerator[LayoutNode](tree, nodeExtentProvider, configuration)
    val layout = treeLayout.getNodeBounds.toMap.map { kv ⇒
      val nodeId = kv._1.index
      val nodeBounds = kv._2
      nodeId -> nodeBounds
    }

    // Shift the entire tree up by one level, since we ignore the ghost root
    def shiftBounds(rectangle: Rectangle2D, yOffset: Double): Rectangle2D = {
      rectangle.setFrame(rectangle.getX, rectangle.getY + yOffset, rectangle.getWidth, rectangle.getHeight)
      rectangle
    }
    val levelHeight = params.nodeHeight + params.levelGap
    val positions = layout.mapValues(nodeBounds ⇒ shiftBounds(nodeBounds, -levelHeight))
    val positionsWithoutGhostRoot = positions.filter(_._1 >= 0)
    val directedEdges = NetworkGraph.findSubordinateEdges(network.graph)
    val undirectedEdges = NetworkGraph.findDistinctUndirectedEdges(network.graph)
    val topManagerEdges = NetworkGraph.findTopManagerEdges(network.graph)
    TreeLayout(positionsWithoutGhostRoot, directedEdges, undirectedEdges ++ topManagerEdges)
  }
}
