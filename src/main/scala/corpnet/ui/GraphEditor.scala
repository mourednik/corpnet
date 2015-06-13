package corpnet.ui

import java.awt.event.{InputEvent, MouseEvent}
import java.awt.geom.Rectangle2D
import java.awt.{Color, Component, Dimension, Paint}

import corpnet._
import corpnet.layout.NodeStyle.NodeStyle
import corpnet.layout._
import corpnet.measure.ModularityResult
import corpnet.network.{Network, NetworkGraph}
import org.piccolo2d.event._
import org.piccolo2d.extras.nodes.PComposite
import org.piccolo2d.nodes.{PPath, PText}
import org.piccolo2d.{PLayer, PNode}

import scala.collection.mutable
import scala.swing._

case class NodeInfo(
  text: String = "",
  textColor: Color = Color.WHITE,
  backgroundColor: Color = GraphEditor.makeColor(1.0f),
  power: Double = 0.0)

trait ScalaPCanvas extends scala.swing.Component {
  override lazy val peer: org.piccolo2d.PCanvas = new org.piccolo2d.PCanvas with SuperMixin
  lazy val camera = peer.getCamera
}

object GraphEditor {

  val nodeIdAttribute = "nodeId"

  // 0.0f minimum, 1.0f maximum
  def makeColor(level: Float): Color = {
    val redFactor = 0.6f
    val greenFactor = 0.7f
    val blueFactor = 1.0f
    val minLevel = 0.35f
    val maxLevel = 1.0f
    val rescaled = (1.0f - level) * (maxLevel - minLevel) + minLevel
    new Color(redFactor * rescaled, greenFactor * rescaled, blueFactor * rescaled)
  }
}

class GraphEditor(
    graphWidth: Int,
    graphHeight: Int,
    dragCallback: Edge ⇒ Unit,
    insertTopManagerCallback: () ⇒ Unit,
    insertSubordinateCallback: Node ⇒ Unit,
    deleteNodeCallback: Node ⇒ Unit) extends ScalaPCanvas {

  import GraphEditor._

  val directedEdgeColor = Color.black
  val undirectedEdgeColor = new Color(200, 160, 0)

  val nodeLayer = peer.getLayer
  val edgeLayer = new PLayer
  val nodeToGuiNode = mutable.Map.empty[Node, PNode]
  val edgeToGuiEdge = mutable.Map.empty[Edge, PPath]

  peer.setPreferredSize(new Dimension(graphWidth, graphHeight))
  edgeLayer.setOffset(graphWidth / 2, graphHeight / 2)
  nodeLayer.setOffset(graphWidth / 2, graphHeight / 2)
  peer.getRoot.addChild(edgeLayer)
  camera.addLayer(0, edgeLayer)
  createEventListener()

  def resetCamera(): Unit = {
    camera.setViewScale(1.0)
    camera.setViewOffset(0.0, 0.0)
  }

  def drawForceLayout(layout: ForceLayout, powers: Option[NodePower],
    nodeStyle: NodeStyle, modularityResult: Option[ModularityResult]): Unit = {
    clearAll()

    val nodeInfo = powers match {
      case Some(powers) ⇒ makeNodeInfoMap(powers, nodeStyle, modularityResult)
      case None         ⇒ Map.empty[Node, NodeInfo].withDefaultValue(NodeInfo())
    }

    if (layout.positions.nonEmpty) {
      layout.positions.toList.foreach {
        case (nodeId, position) ⇒
          drawForceNode(nodeId, position, layout.nodeSize, nodeInfo(nodeId), layout.topManagers.contains(nodeId))
      }
      layout.directedEdges.foreach(edge ⇒ drawForceEdge(edge, directedEdgeColor))
      layout.undirectedEdges.foreach(edge ⇒ drawForceEdge(edge, undirectedEdgeColor))
    }
    camera.repaint()
  }

  def clearAll(): Unit = {
    nodeLayer.removeAllChildren()
    edgeLayer.removeAllChildren()
    nodeToGuiNode.clear()
    edgeToGuiEdge.clear()
  }

  def drawForceNode(nodeId: Node, position: Position, scale: Double, info: NodeInfo, isManager: Boolean): Unit = {
    val size = (0.5 + info.power) * scale
    val pNode =
      if (isManager)
        PPath.createRectangle(position.x, position.y, size, size)
      else
        PPath.createEllipse(position.x, position.y, size, size)
    pNode.addAttribute(nodeIdAttribute, nodeId)
    pNode.setPaint(info.backgroundColor)
    nodeLayer.addChild(pNode)
    nodeToGuiNode += nodeId -> pNode
  }

  def drawForceEdge(edge: Edge, color: Color): Unit = {
    val node1Bounds = nodeToGuiNode(edge._1).getFullBoundsReference
    val node2Bounds = nodeToGuiNode(edge._2).getFullBoundsReference

    val startX = node1Bounds.getCenterX
    val startY = node1Bounds.getCenterY
    val endX = node2Bounds.getCenterX
    val endY = node2Bounds.getCenterY

    val pEdge = new PPath.Float()
    pEdge.reset()
    pEdge.setStrokePaint(color)
    pEdge.moveTo(startX, startY)
    pEdge.lineTo(endX, endY)

    edgeLayer.addChild(pEdge)
    edgeToGuiEdge += edge -> pEdge
  }

  private def makeNodeInfoMap(powers: NodePower,
    nodeStyle: NodeStyle, modularityResult: Option[ModularityResult]): Map[Node, NodeInfo] =
    nodeStyle match {
      case NodeStyle.power     ⇒ makePowerNodeInfoMap(powers)
      case NodeStyle.community ⇒ makeCommunityNodeInfoMap(modularityResult, powers)
    }

  private def makeCommunityNodeInfoMap(modularity: Option[ModularityResult], powers: NodePower): Map[Node, NodeInfo] =
    modularity match {
      case Some(modularity) ⇒
        powers.map {
          case (node, power) ⇒
            val color = modularity.colors.getOrElse(node, Color.WHITE)
            node -> NodeInfo(backgroundColor = color, power = power)
        }
      case None ⇒
        powers.map {
          case (node, power) ⇒ node -> NodeInfo(power = powers(node))
        }
    }

  private def makePowerNodeInfoMap(powers: NodePower): Map[Node, NodeInfo] = {
    val maxPower = powers.values.max.toFloat
    val minPower = powers.values.min.toFloat
    val powerRange = maxPower - minPower
    val normalizedPowers =
      if (powerRange > 0.0f)
        powers.mapValues(power ⇒ (power.toFloat - minPower) / powerRange)
      else
        powers.mapValues(power ⇒ power.toFloat)
    val backgroundColors = normalizedPowers.mapValues(pow ⇒ makeColor(pow))
    val textColors = normalizedPowers.mapValues(level ⇒ if (level >= 0.5) Color.WHITE else Color.BLACK)
    val text = powers.mapValues(pow ⇒ "%2.2f".format(pow))
    powers.map {
      case (nodeId, _) ⇒
        nodeId -> NodeInfo(text(nodeId), textColors(nodeId), backgroundColors(nodeId), powers(nodeId))
    }
  }

  def drawTreeLayout(layout: TreeLayout, powers: Option[NodePower],
    nodeStyle: NodeStyle, modularityResult: Option[ModularityResult]): Unit = {
    clearAll()

    val nodeInfo = powers match {
      case Some(powers) ⇒ makeNodeInfoMap(powers, nodeStyle, modularityResult)
      case None         ⇒ Map.empty[Node, NodeInfo].withDefaultValue(NodeInfo())
    }

    if (layout.positions.nonEmpty) {
      layout.positions.foreach { case (nodeId, position) ⇒ drawTreeNode(nodeInfo, nodeId, position) }
      layout.directedEdges.foreach(edge ⇒ drawTreeEdge(edge, directedEdgeColor))
      layout.undirectedEdges.foreach(edge ⇒ drawTreeEdge(edge, undirectedEdgeColor))
    }
  }

  private def drawTreeNode(info: Map[Node, NodeInfo], nodeId: Node, position: Rectangle2D): Unit = {
    val pNode = PPath.createRectangle(position.getX, position.getY, position.getWidth, position.getHeight)
    pNode.setPaint(info(nodeId).backgroundColor)

    val textNode = new PText()
    textNode.setText(info(nodeId).text)
    textNode.setTextPaint(info(nodeId).textColor)
    textNode.setBounds(pNode.getBounds)
    textNode.setOffset(5.0, 2.5)
    textNode.setHorizontalAlignment(Component.CENTER_ALIGNMENT)

    val compositeNode = new PComposite()
    compositeNode.addChild(pNode)
    compositeNode.addChild(textNode)
    compositeNode.addAttribute(nodeIdAttribute, nodeId)

    nodeLayer.addChild(compositeNode)
    nodeToGuiNode += nodeId -> compositeNode
  }

  private def drawTreeEdge(edge: Edge, color: Color): Unit =
    if (edge._1 != edge._2)
      drawTreeNonLoopEdge(edge, color)
    else
      drawTreeLoopEdge(edge, color)

  private def drawTreeLoopEdge(edge: Edge, color: Color): Unit = {
    val bounds = nodeToGuiNode(edge._1).getFullBoundsReference
    val loopSize = bounds.getHeight
    val x = bounds.getCenterX - loopSize / 2.0
    val y = bounds.getMinY - loopSize
    val loop = PPath.createEllipse(x, y, loopSize, loopSize)
    loop.setStrokePaint(color)
    loop.setPaint(null)
    edgeLayer.addChild(loop)
    edgeToGuiEdge += edge -> loop
  }

  private def drawTreeNonLoopEdge(edge: Edge, color: Color): Unit = {
    val pEdge = new PPath.Float()
    edgeLayer.addChild(pEdge)
    edgeToGuiEdge += edge -> pEdge

    val node1Bounds = nodeToGuiNode(edge._1).getFullBoundsReference
    val node2Bounds = nodeToGuiNode(edge._2).getFullBoundsReference
    pEdge.reset()

    pEdge.setStrokePaint(color)

    val nodeDistance = node1Bounds.getCenter2D.getY - node2Bounds.getCenter2D.getY

    nodeDistance match {
      case dist if dist == 0 ⇒
        // same level
        val startX = node1Bounds.getCenterX
        val startY = node1Bounds.getMaxY
        val endX = node2Bounds.getCenterX
        val endY = node2Bounds.getMaxY
        val curveSize = 20.0
        val midX = (startX + endX) / 2
        pEdge.setPaint(null) // makes the curve background transparent
        pEdge.moveTo(startX, startY)
        pEdge.curveTo(midX, startY + curveSize, midX, endY + curveSize, endX, endY)
      case dist if dist > 0 ⇒
        // node1 is below
        val startX = node1Bounds.getCenterX
        val startY = node1Bounds.getMinY
        val endX = node2Bounds.getCenterX
        val endY = node2Bounds.getMaxY
        pEdge.moveTo(startX, startY)
        pEdge.lineTo(endX, endY)
      case dist if dist < 0 ⇒
        // node2 is below
        val startX = node1Bounds.getCenterX
        val startY = node1Bounds.getMaxY
        val endX = node2Bounds.getCenterX
        val endY = node2Bounds.getMinY
        pEdge.moveTo(startX, startY)
        pEdge.lineTo(endX, endY)
    }
  }

  private def createEventListener(): Unit = {
    nodeLayer.addInputEventListener(
      new DragEventHandler(dragCallback))
    nodeLayer.addInputEventListener(
      new NodeInputEventHandler(insertSubordinateCallback, deleteNodeCallback))
    camera.addInputEventListener(new CameraInputEventHandler(insertTopManagerCallback))
    val mouseZoomHandler = new PMouseWheelZoomEventHandler
    mouseZoomHandler.setScaleFactor(mouseZoomHandler.getScaleFactor * -1)
    camera.addInputEventListener(mouseZoomHandler)
  }
}

class NodeInputEventHandler(
    insertSubordinateCallback: Node ⇒ Unit,
    deleteNodeCallback: Node ⇒ Unit) extends PBasicInputEventHandler {

  override def mouseClicked(event: PInputEvent): Unit = {
    val node = event.getPickedNode
    val nodeId = node.getAttribute(GraphEditor.nodeIdAttribute).asInstanceOf[Node]

    event.getButton match {
      case 1 ⇒ // left button
        insertSubordinateCallback(nodeId)
      case 3 ⇒ // right button
        deleteNodeCallback(nodeId)
      case _ ⇒
    }
  }
}

class CameraInputEventHandler(insertTopManagerCallback: () ⇒ Unit) extends PBasicInputEventHandler {
  override def mouseClicked(event: PInputEvent): Unit = {
    event.getButton match {
      case 1 if event.getPickedNode == event.getTopCamera ⇒
        // left button
        insertTopManagerCallback()
      case _ ⇒
    }
  }
}

class DragEventHandler(dragCallback: Edge ⇒ Unit) extends PDragSequenceEventHandler {

  val nodeHighlightColor = Color.RED

  var startNode: Option[PNode] = None
  var startNodeColor: Option[Paint] = None
  var lastEnteredNode: Option[PNode] = None
  var lastEnteredNodeColor: Option[Paint] = None

  setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK))

  def getColorableNode(node: PNode): PNode = {
    node match {
      case compositeNode: PComposite ⇒
        compositeNode.getChild(0)
      case nonCompositeNode ⇒
        nonCompositeNode
    }
  }

  override def mouseEntered(e: PInputEvent): Unit = {
    super.mouseEntered(e)
    if (e.getButton == MouseEvent.NOBUTTON) {
      val node = e.getPickedNode
      lastEnteredNode = Some(node)
      lastEnteredNodeColor = Some(getColorableNode(node).getPaint)
      getColorableNode(node).setPaint(nodeHighlightColor)
    }
  }

  override def mouseExited(e: PInputEvent): Unit = {
    super.mouseExited(e)
    if (isDragging && lastEnteredNode.get != startNode.get) {
      // don't reset start node color
      for {
        lastEntered ← lastEnteredNode
        lastEnteredColor ← lastEnteredNodeColor
      } {
        getColorableNode(lastEntered).setPaint(lastEnteredColor)
      }
    } else {
      for {
        lastEntered ← lastEnteredNode
        lastEnteredColor ← lastEnteredNodeColor
      } {
        getColorableNode(lastEntered).setPaint(lastEnteredColor)
      }
    }
    lastEnteredNode = None
  }

  protected override def shouldStartDragInteraction(event: PInputEvent): Boolean =
    super.shouldStartDragInteraction(event) && event.getPickedNode.ne(event.getTopCamera)

  protected override def startDrag(e: PInputEvent): Unit = {
    super.startDrag(e)
    startNode = lastEnteredNode
    startNodeColor = lastEnteredNodeColor
    e.setHandled(true)
  }

  protected override def endDrag(event: PInputEvent): Unit = {
    super.endDrag(event)
    for {
      start ← startNode
      startColor ← startNodeColor
      lastEntered ← lastEnteredNode
    } {
      val startNodeId = start.getAttribute(GraphEditor.nodeIdAttribute).asInstanceOf[Node]
      val endNodeId = lastEntered.getAttribute(GraphEditor.nodeIdAttribute).asInstanceOf[Node]
      if (startNodeId != endNodeId) {
        val edge = (startNodeId, endNodeId)
        dragCallback(edge)
      }

      getColorableNode(start).setPaint(startColor)
      startNode = None
      startNodeColor = None
    }
  }
}

object GraphEditorTester extends SimpleSwingApplication {
  val graph1 = NetworkGraph()
  val (graph2, topManager1) = NetworkGraph.insertTopManager(graph1)
  val (graph3, topManager2) = NetworkGraph.insertTopManager(graph2)
  val (graph4, child1) = NetworkGraph.insertSubordinate(graph3, topManager1)
  val (graph5, child2) = NetworkGraph.insertSubordinate(graph4, topManager1)
  val (graph6, child3) = NetworkGraph.insertSubordinate(graph5, topManager1)
  val (graph7, child4) = NetworkGraph.insertSubordinate(graph6, child1)
  val graph8 = NetworkGraph.insertUndirectedEdge(graph7, child1, child2)
  val graph9 = NetworkGraph.insertUndirectedEdge(graph8, child2, child3)
  val graph10 = NetworkGraph.insertUndirectedEdge(graph9, child1, child3)
  val graph11 = NetworkGraph.insertUndirectedEdge(graph10, topManager2, child4)
  val powers = Map(
    topManager1 -> 1.5,
    topManager2 -> 2.0,
    child1 -> 1.0,
    child2 -> 0.8,
    child3 -> 0.5,
    child4 -> 0.4)
  val network = Network().copy(graph = graph11)
  val treeLayout = TreeLayoutBuilder.buildTreeLayout(network, TreeLayoutParameters())
  val forceLayout = ForceLayoutBuilder.build(network.graph, ForceLayoutParameters(), None)
  val dragCallback = (edge: Edge) ⇒ println(s"dragged edge $edge")
  val insertTopManagerCallback = () ⇒ println("insert top manager")
  val insertSubordinateCallback = (node: Node) ⇒ println(s"insert subordinate to $node")
  val deleteNodeCallback = (node: Node) ⇒ println(s"delete node $node")

  val graphEditor = new GraphEditor(
    800, 800, dragCallback, insertTopManagerCallback, insertSubordinateCallback, deleteNodeCallback)

  def top = new MainFrame {
    contents = new BorderPanel {

      import BorderPanel.Position._

      layout(graphEditor) = Center
    }
    //graphEditor.drawTreeLayout(treeLayout, Some(powers))
    graphEditor.drawForceLayout(forceLayout, Some(powers), NodeStyle.power, None)
  }

  println(forceLayout)
}

