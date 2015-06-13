package corpnet.ui

import corpnet.generator.SocialNetworkParameters

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.util.{Failure, Success, Try}

class SocialNetworkDialog(params: SocialNetworkParameters) extends Dialog {
  var parameters: Option[SocialNetworkParameters] = None
  val pField = new TextField(params.p.toString)
  val densityField = new TextField(params.density.toString)
  val deleteEdgesCheckbox = new CheckBox() {
    selected = true
  }

  title = "Generate a Social Network"
  modal = true

  val generateButton = Button("Generate") {
    Try {
      val p = pField.text.toDouble
      val density = densityField.text.toInt
      if (p <= 0) throw new Exception("p <= 0")
      if (p > 1.0) throw new Exception("p > 1.0")
      if (density <= 0) throw new Exception("density <= 0")
      SocialNetworkParameters(p, density, deleteEdgesCheckbox.selected)
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

    contents += new Label("p:")
    contents += pField
    contents += new Label("Density:")
    contents += densityField
    contents += new Label("Delete existing edges:")
    contents += deleteEdgesCheckbox
  }

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(generateButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  centerOnScreen()
  open()
}
