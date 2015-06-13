package corpnet.measure

import corpnet.network.Network
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source._

class ModularityTest extends FunSpec with Matchers {

  describe("Modularity") {
    it("works") {
      new PresetRandomFixture {
        val weightedEdges = Network.findWeightedEdges(network, false, false, true, true)
        println(Modularity.run(weightedEdges))
      }
    }
  }
  trait PresetRandomFixture {

    import Network.networkProtocol._

    val filename = "src/test/resources/random_279_nodes.network"
    val text = fromFile(filename).getLines().mkString("\n")
    val network = fromJsonString(text)
  }
}
