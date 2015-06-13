package corpnet.ui

import corpnet.measure.ModularityParameters

import scala.swing.BorderPanel.Position._
import scala.swing._

class ModularityDialog(params: ModularityParameters) extends Dialog {
  var parameters: Option[ModularityParameters] = None

  val topManagerEdgesCheckBox = new CheckBox() {
    selected = params.topManagers
  }

  val subordinateEdgesCheckBox = new CheckBox() {
    selected = params.subordinates
  }

  val backflowEdgesCheckBox = new CheckBox() {
    selected = params.backflow
  }

  val undirectedEdgesCheckBox = new CheckBox() {
    selected = params.undirected
  }

  title = "Calculate Modularity"
  modal = true

  val calculateButton = Button("Calculate") {
    parameters = Some(ModularityParameters(
      topManagers = topManagerEdgesCheckBox.selected,
      subordinates = subordinateEdgesCheckBox.selected,
      backflow = backflowEdgesCheckBox.selected,
      undirected = undirectedEdgesCheckBox.selected))
    close()
  }

  val cancelButton = Button("Cancel") {
    close()
  }

  val inputFieldsPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(5, 5, 5, 5)
    contents += new Label("Top manager edges:")
    contents += topManagerEdgesCheckBox
    contents += new Label("Subordinate edges:")
    contents += subordinateEdgesCheckBox
    contents += new Label("Backflow edges:")
    contents += backflowEdgesCheckBox
    contents += new Label("Undirected edges:")
    contents += undirectedEdgesCheckBox
  }

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(calculateButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  centerOnScreen()
  open()
}
