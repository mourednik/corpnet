package corpnet.ui

import java.nio.file.Path

import corpnet.io.CustomFileChooser
import corpnet.measure.ModularityParameters

import scala.swing.BorderPanel.Position._
import scala.swing._

class TSVExportDialog(params: ModularityParameters) extends Dialog {
  var parameters: Option[ModularityParameters] = None
  var outputFile: Option[Path] = None

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

  title = "Export to TSV"
  modal = true

  val tsvFileChooser = new CustomFileChooser(".tsv")
  val exportButton = Button("Export") {
    parameters = Some(ModularityParameters(
      topManagers = topManagerEdgesCheckBox.selected,
      subordinates = subordinateEdgesCheckBox.selected,
      backflow = backflowEdgesCheckBox.selected,
      undirected = undirectedEdgesCheckBox.selected))
    outputFile = tsvFileChooser.saveDialog().map(_.toPath)
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

  val buttonPanel = new FlowPanel(FlowPanel.Alignment.Right)(exportButton, cancelButton)

  contents = new BorderPanel {
    layout(inputFieldsPanel) = Center
    layout(buttonPanel) = South
  }

  centerOnScreen()
  open()
}
