package corpnet.ui

import corpnet.generator.RandomTreeParameters
import corpnet.network.NetworkParameters

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.util.{Failure, Success, Try}

class RandomTreeDialog(params: RandomTreeParameters, netParams: NetworkParameters) extends Dialog {
  var parameters: Option[RandomTreeParameters] = None
  val initialMeanField = new TextField(params.initialMean.toString)
  val stdDevField = new TextField(params.standardDeviation.toString)
  val meanIncrementField = new TextField(params.meanIncrementPerLevel.toString)
  val heightField = new TextField(params.height.toString)
  val freeCapacityField = new TextField(params.freeCapacity.toString)
  val wField = new TextField(netParams.w.toString)
  val kField = new TextField(netParams.k.toString)
  val tField = new TextField(netParams.t.toString)

  title = "Generate a Random Tree"
  modal = true

  val generateButton = Button("Generate") {
    Try {
      val initialMean = initialMeanField.text.toDouble
      val stdDev = stdDevField.text.toDouble
      val meanIncrement = meanIncrementField.text.toDouble
      val height = heightField.text.toInt
      val freeCapacity = freeCapacityField.text.toDouble
      val w = wField.text.toDouble
      val k = kField.text.toDouble
      val t = tField.text.toDouble
      if (height <= 0) throw new Exception("height <= 0")
      if (freeCapacity < 0) throw new Exception("freeCapacity < 0")
      RandomTreeParameters(initialMean, stdDev, meanIncrement, height, freeCapacity, w, k, t)
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

    contents += new Label("Initial mean number of children:")
    contents += initialMeanField
    contents += new Label("Standard deviation:")
    contents += stdDevField
    contents += new Label("Mean increment per level:")
    contents += meanIncrementField
    contents += new Label("Height:")
    contents += heightField
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
