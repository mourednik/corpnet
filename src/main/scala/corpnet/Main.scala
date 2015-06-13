package corpnet

import javax.swing.text.DefaultCaret

import akka.actor.{ActorSystem, Props}
import corpnet.io.CustomFileChooser
import corpnet.layout._
import corpnet.ui._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.{Failure, Success, Try}

object Main extends SimpleSwingApplication {

  import BackendFSMProtocol._

  val defaultGuiWidth: Int = 800
  val defaultGuiHeight: Int = 800
  val defaultDividerLocation: Int = 700
  val maxUndoLevels: Int = 20

  val system = ActorSystem()
  val kernel = system.actorOf(Props(classOf[BackendFSM]))
  var state = ApplicationState()

  val graphEditor = new GraphEditor(defaultGuiWidth, defaultGuiHeight,
    dragCallback, insertTopManagerCallback, insertSubordinateCallback, deleteNodeCallback)

  val logTextArea = new TextArea {
    editable = false
  }
  logTextArea.peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)

  Logger.registerHandler(println)
  Logger.registerHandler(text ⇒ logTextArea.append(text + "\n"))

  def deleteNodeCallback(node: Node): Unit = kernel ! DeleteNode(node)

  def dragCallback(edge: Edge): Unit = kernel ! DragEdge(edge)

  def insertSubordinateCallback(node: Node): Unit = kernel ! InsertSubordinate(node)

  def insertTopManagerCallback(): Unit = kernel ! InsertTopManager

  def printUsage(): Unit = Logger.info(
    """
      |Usage:
      | Insert a new top manager: left click on background.
      | Insert a new subordinate to a node: left click on node.
      | Delete a node: right click on node.
      | Insert/delete a non-reporting edge: drag from node1 to node2 with left button.
      | Zoom UI: Use the mousewheel, or hold right mouse button and move mouse left or right.
    """.stripMargin.trim)

  def checkIfUsingNativeNumericLibs(): Unit = {
    val usingNatives = com.github.fommil.netlib.BLAS.getInstance.getClass.getName != "com.github.fommil.netlib.F2jBLAS"
    if (usingNatives)
      Logger.info("Using native linear algebra library")
    else
      Logger.warn("Not using linear algebra library")
  }

  def changeState(state: ApplicationState): Unit = this.state = state

  def redraw(): Future[Unit] = {
    val f = Future {
      Logger.debug("Redrawing graph...")
      val powers = state.bonacichPower.map(_.nodePower)
      state.layoutStyle match {
        case LayoutStyle.Tree ⇒
          state.treeLayout match {
            case Some(layout) ⇒ graphEditor.drawTreeLayout(layout, powers, state.nodeStyle, state.modularityResult)
            case None         ⇒ graphEditor.clearAll()
          }
        case LayoutStyle.Force ⇒
          state.forceLayout match {
            case Some(layout) ⇒ graphEditor.drawForceLayout(layout, powers, state.nodeStyle, state.modularityResult)
            case None         ⇒ graphEditor.clearAll()
          }
      }
    }
    f onComplete {
      case Success(_) ⇒ Logger.debug("Finished redrawing graph")
      case Failure(e) ⇒ Logger.error(s"Redraw graph failed: ${e.getMessage}")
    }
    f
  }

  def top = new MainFrame {
    title = "Network Graph Editor"

    val networkFileChooser = new CustomFileChooser(".network")

    val autoRecalculateMenuItem = new CheckMenuItem("Recalculate node powers on update") {
      selected = true
    }
    val treeItem = new RadioMenuItem("Tree")
    val forceItem = new RadioMenuItem("Force")
    val layoutSelectMutex = new ButtonGroup(treeItem, forceItem)
    state.layoutStyle match {
      case LayoutStyle.Force ⇒ layoutSelectMutex.select(forceItem)
      case LayoutStyle.Tree  ⇒ layoutSelectMutex.select(treeItem)
    }

    val nodeStylePowerItem = new RadioMenuItem("Power")
    val nodeStyleCommunityItem = new RadioMenuItem("Community")
    val nodeStyleSelectMutex = new ButtonGroup(nodeStylePowerItem, nodeStyleCommunityItem)
    state.nodeStyle match {
      case NodeStyle.community ⇒ nodeStyleSelectMutex.select(nodeStyleCommunityItem)
      case NodeStyle.power     ⇒ nodeStyleSelectMutex.select(nodeStylePowerItem)
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("New") {
          kernel ! NewNetwork
        })
        contents += new MenuItem(Action("Open") {
          networkFileChooser.openDialog(this) match {
            case Some(file) ⇒ kernel ! LoadNetwork(file)
            case None       ⇒
          }
        })
        contents += new MenuItem(Action("Save as") {
          networkFileChooser.saveDialog(this) match {
            case Some(file) ⇒ kernel ! SaveNetwork(file)
            case None       ⇒
          }
        })
        contents += new MenuItem(Action("Export to TSV") {
          val dialog = new TSVExportDialog(state.modularityParameters)
          (dialog.parameters, dialog.outputFile) match {
            case (Some(parameters), Some(path)) ⇒ kernel ! ExportTSV(path, parameters)
            case _                              ⇒
          }

        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
      contents += new Menu("Edit") {
        contents += new MenuItem(Action("Undo") {
          kernel ! Undo
        })
      }
      contents += new Menu("Settings") {
        contents += new MenuItem(Action("Network") {
          new NetworkParametersDialog(state.network.parameters).parameters match {
            case Some(parameters) ⇒ kernel ! ConfigureNetwork(parameters)
            case None             ⇒
          }
        })
        contents += new MenuItem(Action("Force layout") {
          new ForceLayoutDialog(state.forceLayoutParameters).parameters match {
            case Some(parameters) ⇒ kernel ! ConfigureForceLayout(parameters)
            case None             ⇒
          }
        })
        contents += new Separator
        contents += autoRecalculateMenuItem
      }
      contents += new Menu("Generate") {
        contents += new MenuItem(Action("Perfect Tree") {
          new PerfectTreeDialog(state.perfectTreeParameters, state.network.parameters).parameters match {
            case Some(parameters) ⇒ kernel ! GeneratePerfectTree(parameters)
            case None             ⇒
          }
        })
        contents += new MenuItem(Action("Random Tree") {
          new RandomTreeDialog(state.randomTreeParameters, state.network.parameters).parameters match {
            case Some(parameters) ⇒ kernel ! GenerateRandomTree(parameters)
            case None             ⇒
          }
        })
        contents += new MenuItem(Action("Social Network") {
          new SocialNetworkDialog(state.socialNetworkParameters).parameters match {
            case Some(parameters) ⇒ kernel ! GenerateSocialNetwork(parameters)
            case None             ⇒
          }
        })
      }
      contents += new Menu("Analyze") {
        contents += new MenuItem(Action("Calculate node powers") {
          kernel ! RecalculatePowers
        })
        contents += new MenuItem(Action("Modularity") {
          new ModularityDialog(state.modularityParameters).parameters match {
            case Some(parameters) ⇒ kernel ! CalculateModularity(parameters)
            case None             ⇒
          }
        })
      }
      contents += new Menu("Visualize") {
        contents ++= layoutSelectMutex.buttons
        contents += new Separator
        contents += new Menu("Chart") {
          contents += new MenuItem(Action("Descending node powers grouped by level") {
            state.bonacichPower match {
              case Some(bonacichPower) ⇒ kernel ! PlotPowers
              case None                ⇒
            }
          })
          contents += new MenuItem(Action("Power Histogram") {
            state.bonacichPower match {
              case Some(bonacichPower) ⇒
                import Dialog._
                val s = showInput(title = "Histogram", message = "Enter the number of bins", initial = "15")
                s.flatMap(str ⇒ Try(str.toInt).toOption) match {
                  case Some(n) ⇒ kernel ! PlotHistogram(n)
                  case None    ⇒ Logger.error("Invalid input for histogram bins")
                }
              case None ⇒
            }
          })
        }
        contents += new Menu("Nodes") {
          contents ++= nodeStyleSelectMutex.buttons
        }
        contents += new Separator
        contents += new MenuItem(Action("Reset view") {
          graphEditor.resetCamera()
        })
      }
      contents += new Menu("Help") {
        contents += new MenuItem(Action("Usage") {
          printUsage()
        })
      }
    }

    val logTextAreaScrollPane = new ScrollPane(logTextArea)

    contents = new BorderPanel {

      import BorderPanel.Position._

      val splitPane = new SplitPane(Orientation.Horizontal, graphEditor, logTextAreaScrollPane) {
        oneTouchExpandable = true
        continuousLayout = true
        dividerLocation = defaultDividerLocation
      }
      layout(splitPane) = Center
    }

    listenTo(autoRecalculateMenuItem)
    listenTo(forceItem)
    listenTo(treeItem)
    listenTo(nodeStyleCommunityItem)
    listenTo(nodeStylePowerItem)

    reactions += {
      case r: ButtonClicked ⇒
        r.source match {
          case `autoRecalculateMenuItem` ⇒ kernel ! SetRecalculateOption(autoRecalculateMenuItem.selected)
          case `forceItem`               ⇒ kernel ! SetLayoutStyle(LayoutStyle.Force)
          case `treeItem`                ⇒ kernel ! SetLayoutStyle(LayoutStyle.Tree)
          case `nodeStyleCommunityItem`  ⇒ kernel ! SetNodeStyle(NodeStyle.community)
          case `nodeStylePowerItem`      ⇒ kernel ! SetNodeStyle(NodeStyle.power)
        }
      case _ ⇒
    }

    checkIfUsingNativeNumericLibs()
    printUsage()
  }
}
