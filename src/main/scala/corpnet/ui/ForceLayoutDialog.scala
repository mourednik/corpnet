package corpnet.ui

import corpnet.layout.{EdgeSelection, ForceConstants, ForceLayoutParameters}

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.SelectionChanged
import scala.util.{Failure, Success, Try}

class ForceLayoutDialog(val currentParams: ForceLayoutParameters) extends Dialog {
  var parameters: Option[ForceLayoutParameters] = None

  val iterationsField = new TextField(currentParams.iterations.toString)
  val edgeOptions = IndexedSeq(EdgeSelection.All, EdgeSelection.Directed, EdgeSelection.Undirected)
  val edgeComboBox = new ComboBox(edgeOptions)
  edgeComboBox.peer.setSelectedItem(currentParams.edgeSelection)

  title = "Force Layout Parameters"
  modal = true

  def loadParams(): Unit = {
    val constants = edgeComboBox.selection.item match {
      case EdgeSelection.All ⇒
        ForceConstants.optimizedAllEdges()
      case EdgeSelection.Directed ⇒
        ForceConstants.optimizedDirected()
      case EdgeSelection.Undirected ⇒
        ForceConstants.optimizedUndirected()
    }
  }

  val okButton = Button("OK") {
    Try {
      val iterations = iterationsField.text.toInt
      val edgeSelection = edgeComboBox.selection.item
      if (iterations <= 0) throw new RuntimeException("iterations <= 0")
      ForceLayoutParameters(currentParams.constants, iterations, edgeSelection)
    } match {
      case Success(result) ⇒
        parameters = Some(result)
        close()
      case Failure(e) ⇒
        Dialog.showMessage(null, e.getMessage, "Invalid input values", Dialog.Message.Error)
    }
  }

  val cancelButton = Button("Cancel") {
    close()
  }

  val inputFieldsPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    contents += new Label("Iterations:")
    contents += iterationsField
    contents += new Label("Edges:")
    contents += edgeComboBox
  }

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(okButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  listenTo(edgeComboBox.selection)

  reactions += {
    case SelectionChanged(`edgeComboBox`) ⇒ loadParams()
    case _                                ⇒
  }

  centerOnScreen()
  open()
}

