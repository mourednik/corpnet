package corpnet.ui

import corpnet.generator.PerfectTreeParameters
import corpnet.network.NetworkParameters

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.util.{Failure, Success, Try}

class PerfectTreeDialog(treeParams: PerfectTreeParameters, netParams: NetworkParameters) extends Dialog {
  var parameters: Option[PerfectTreeParameters] = None
  val heightField = new TextField(treeParams.height.toString)
  val childrenPerNodeField = new TextField(treeParams.childrenPerNode.toString)
  val freeCapacityField = new TextField(treeParams.freeCapacity.toString)
  val wField = new TextField(netParams.w.toString)
  val kField = new TextField(netParams.k.toString)
  val tField = new TextField(netParams.t.toString)

  title = "Generate a Perfect Tree"
  modal = true

  val generateButton = Button("Generate") {
    Try {
      val height = heightField.text.toInt
      val childrenPerNode = childrenPerNodeField.text.toInt
      val freeCapacity = freeCapacityField.text.toInt
      val w = wField.text.toDouble
      val k = kField.text.toDouble
      val t = tField.text.toDouble
      if (height <= 0) throw new Exception("height <= 0")
      if (childrenPerNode <= 0) throw new Exception("childrenPerNode <= 0")
      if (freeCapacity < 0) throw new Exception("freeCapacity < 0")
      PerfectTreeParameters(height, childrenPerNode, freeCapacity, w, k, t)
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

    contents += new Label("Height:")
    contents += heightField
    contents += new Label("Children per node:")
    contents += childrenPerNodeField
    contents += new Label("Unused capacity:")
    contents += freeCapacityField
    contents += new Label("w")
    contents += wField
    contents += new Label("k")
    contents += kField
    contents += new Label("t")
    contents += tField
  }

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(generateButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  centerOnScreen()
  open()
}
