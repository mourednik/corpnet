package corpnet.io

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import javax.swing.filechooser.FileFilter

import corpnet.Edge
import corpnet.measure.ModularityParameters
import corpnet.network.{Network, WeightedEdge}

import scala.swing.FileChooser.Result
import scala.swing.{Component, FileChooser}

class CustomFileChooser(val fileExtension: String = ".network") extends FileChooser {
  fileFilter = new FileFilter {
    override def getDescription: String = s"$fileExtension file"

    override def accept(file: File): Boolean = file.isDirectory || file.getName.endsWith(fileExtension)
  }

  def saveDialog(over: Component = null): Option[File] =
    showSaveDialog(over) match {
      case Result.Approve ⇒
        if (selectedFile.getName.endsWith(fileExtension))
          Some(selectedFile)
        else
          Some(new File(selectedFile.getPath + fileExtension))
      case _ ⇒
        None
    }

  def openDialog(over: Component): Option[File] =
    showOpenDialog(over) match {
      case Result.Approve ⇒ Some(selectedFile)
      case _              ⇒ None
    }
}

object IOUtil {

  def save(network: Network, file: File): Path = {
    import Network.networkProtocol._
    val jsonString = toJsonString(network)
    save(jsonString, file.toPath)
  }

  def loadNetwork(file: File): Network = {
    import Network.networkProtocol._
    val string = scala.io.Source.fromFile(file).getLines().mkString("\n")
    fromJsonString(string)
  }

  def exportTSVToTemp(edges: Set[WeightedEdge]): Path = {
    val path = Files.createTempFile("network", ".tsv")
    exportTSVToPath(edges, path)
  }

  def exportTSVToPath(edges: Set[WeightedEdge], path: Path): Path = {
    val tsv = weightedEdgesToTSV(edges)
    save(tsv, path)
  }

  def exportTSVToTemp(network: Network, parameters: ModularityParameters): Path = {
    val weightedEdges = Network.findWeightedEdges(network, parameters.topManagers, parameters.subordinates,
      parameters.backflow, parameters.undirected)
    exportTSVToTemp(weightedEdges)
  }

  def exportTSVToPath(network: Network, parameters: ModularityParameters, path: Path): Path = {
    val weightedEdges = Network.findWeightedEdges(network, parameters.topManagers, parameters.subordinates,
      parameters.backflow, parameters.undirected)
    exportTSVToPath(weightedEdges, path)
  }

  def save(text: String, path: Path): Path = {
    Files.write(path, text.getBytes(StandardCharsets.UTF_8))
  }

  private def edgeLessThan(edge1: Edge, edge2: Edge): Boolean =
    edge1._1 < edge2._1 || ((edge1._1 == edge2._1) && edge1._2 < edge2._2)

  def edgesToTSV(edges: Set[Edge]): String = {

    val sortedEdges = edges.toList.sortWith(edgeLessThan)

    val tsvRows =
      for {
        (node1, node2) ← sortedEdges
      } yield {
        s"$node1\t$node2\t1.0"
      }

    tsvRows.mkString("\n")
  }

  def weightedEdgesToTSV(edges: Set[WeightedEdge]): String = {

    def weightedEdgeLessThan(edge1: WeightedEdge, edge2: WeightedEdge): Boolean =
      edgeLessThan(edge1.edge, edge2.edge)

    val sortedEdges = edges.toList.sortWith(weightedEdgeLessThan)

    val tsvRows =
      for {
        WeightedEdge((node1, node2), weight) ← sortedEdges
      } yield {
        s"$node1\t$node2\t$weight"
      }

    tsvRows.mkString("\n")
  }

}
