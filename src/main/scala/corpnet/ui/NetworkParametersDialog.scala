package corpnet.ui

import corpnet.network.{ManagementStyle, NetworkParameters}

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.SelectionChanged
import scala.util.{Failure, Success, Try}

class NetworkParametersDialog(initialParameters: NetworkParameters) extends Dialog {
  var parameters: Option[NetworkParameters] = None
  val betaMultiplierField = new TextField(initialParameters.betaMultiplier.toString)
  val deltaField = new TextField(initialParameters.delta.toString)
  val wField = new TextField(initialParameters.w.toString)
  val kField = new TextField(initialParameters.k.toString)
  val tField = new TextField(initialParameters.t.toString)
  val maxDepthField = new TextField(initialParameters.maxDepth.toString)
  val presetOptions = IndexedSeq(
    ManagementStyle.custom, ManagementStyle.autocratic, ManagementStyle.democratic, ManagementStyle.paternalistic,
    ManagementStyle.chaotic)
  val presetComboBox = new ComboBox(presetOptions)
  presetComboBox.peer.setSelectedIndex(0)

  title = "Configure Network Parameters"
  modal = true

  val okButton = Button("OK") {
    Try {
      val betaMultiplier = betaMultiplierField.text.toDouble
      val delta = deltaField.text.toDouble
      val w = wField.text.toDouble
      val s = initialParameters.s
      val k = kField.text.toDouble
      val t = tField.text.toDouble
      val maxDepth = maxDepthField.text.toInt
      NetworkParameters(
        betaMultiplier = betaMultiplier, delta = delta, w = w, s = s, k = k, t = t, maxDepth = maxDepth)
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

    contents += new Label("beta multiplier")
    contents += betaMultiplierField
    contents += new Label("delta")
    contents += deltaField
    contents += new Label("w")
    contents += wField
    contents += new Label("k")
    contents += kField
    contents += new Label("t")
    contents += tField
    contents += new Label("maxDepth")
    contents += maxDepthField
    contents += new Label("management style")
    contents += presetComboBox
  }

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(okButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  def loadParameters(parameters: NetworkParameters): Unit = {
    betaMultiplierField.text = parameters.betaMultiplier.toString
    deltaField.text = parameters.delta.toString
    wField.text = parameters.w.toString
    kField.text = parameters.k.toString
    tField.text = parameters.t.toString
    maxDepthField.text = parameters.maxDepth.toString
  }

  listenTo(presetComboBox.selection)

  reactions += {
    case SelectionChanged(`presetComboBox`) ⇒
      val parameters = presetComboBox.selection.item match {
        case ManagementStyle.autocratic    ⇒ NetworkParameters.autocratic(initialParameters)
        case ManagementStyle.chaotic       ⇒ NetworkParameters.chaotic(initialParameters)
        case ManagementStyle.custom        ⇒ initialParameters
        case ManagementStyle.democratic    ⇒ NetworkParameters.democratic(initialParameters)
        case ManagementStyle.paternalistic ⇒ NetworkParameters.paternalistic(initialParameters)
      }
      loadParameters(parameters)
  }

  centerOnScreen()
  open()
}
