package corpnet.measure

import java.awt.Color

import ch.epfl.lis.jmod.{ Jmod, JmodNetwork }
import ch.epfl.lis.networks.{ Node ⇒ CNode, Structure }
import corpnet.Node
import corpnet.io.IOUtil
import corpnet.network.{ Network, WeightedEdge }

import scala.collection.JavaConversions._

case class ModularityResult(communities: List[Set[Node]], colors: Map[Node, Color], computationTime: Long,
    modularity: Double) {
  override def toString = {
    val numCommunities = communities.size
    val communitySizes = communities.map(_.size).mkString("[", ",", "]")
    val seconds = computationTime / 1000.0f
    f"Modularity=$modularity%2.2f. Found $numCommunities communities with sizes $communitySizes in $seconds%3.2f seconds"
  }
}
case class ModularityParameters(topManagers: Boolean = false, subordinates: Boolean = false, backflow: Boolean = true,
  undirected: Boolean = true)

object Modularity {

  def run(network: Network, parameters: ModularityParameters): Option[ModularityResult] = {
    val weightedEdges = Network.findWeightedEdges(network, parameters.topManagers, parameters.subordinates,
      parameters.backflow, parameters.undirected)
    run(weightedEdges)
  }

  def run(edges: Set[WeightedEdge]): Option[ModularityResult] =
    if (edges.isEmpty)
      None
    else {
      val (jmod, jNetwork) = runModularityDetection(edges)
      val communities = extractCommunities(jmod, jNetwork)
      val colors = colorCommunities(communities)
      val computationTime = jmod.getRootCommunity.getComputationTime
      val modularity = jmod.getModularity
      Some(ModularityResult(communities, colors, computationTime, modularity))
    }

  def makeColors(n: Int, saturation: Float, brightness: Float): List[Color] = {
    val hues = breeze.linalg.linspace(0.0, 1.0, n + 1).toArray.toList.take(n).map(_.toFloat)
    hues.map(hue ⇒ Color.getHSBColor(hue, saturation, brightness))
  }

  def runModularityDetection(edges: Set[WeightedEdge]): (Jmod, JmodNetwork) = {
    val tsvPath = IOUtil.exportTSVToTemp(edges)
    val jNetwork = new JmodNetwork()
    jNetwork.read(tsvPath.toUri, Structure.Format.TSV)
    jNetwork.initializeModularityDetectionVariables()
    val jmod = new Jmod
    jmod.runModularityDetection(jNetwork)
    (jmod, jNetwork)
  }

  def extractCommunities(jmod: Jmod, jmodNetwork: JmodNetwork): List[Set[Node]] = {
    val communities = jmod.getRootCommunity.getIndivisibleCommunities.toList
    for {
      community ← communities
    } yield {
      val indices = community.getVertexIndexes.getData.map(_.toInt)
      jmodNetwork.getNodesFromIndexes(indices).toSet.map((node: CNode) ⇒ node.getName.toInt)
    }
  }

  def colorCommunities(communities: List[Set[Node]]): Map[Node, Color] = {
    val colors = makeColors(communities.size, 0.8f, 0.8f)
    val nodesColors = for {
      (community, color) ← communities zip colors
      node ← community
    } yield {
      node -> color
    }
    nodesColors.toMap
  }

}
