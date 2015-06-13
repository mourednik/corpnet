package corpnet.generator

import corpnet.network.NetworkGraph
import org.scalatest.{FunSpec, Matchers}

class NetworkTreeBuilderTest extends FunSpec with Matchers {

  describe("NetworkTreeBuilder") {

    it("works") {
      new defaultNetworkFixture {
        val expectedChildren = Map(
          NetworkTree.ghostRootID -> Set(topManager),
          topManager -> Set(subordinate1, subordinate2),
          subordinate1 -> Set(subordinate3, subordinate4),
          subordinate2 -> Set(),
          subordinate3 -> Set(),
          subordinate4 -> Set())

        val expectedParent = Map(
          topManager -> Some(NetworkTree.ghostRootID),
          subordinate1 -> Some(topManager),
          subordinate2 -> Some(topManager),
          subordinate3 -> Some(subordinate1),
          subordinate4 -> Some(subordinate1))

        val expectedLevel = Map(
          topManager -> 0,
          subordinate1 -> 1,
          subordinate2 -> 1,
          subordinate3 -> 2,
          subordinate4 -> 2)

        val networkTree = NetworkTreeBuilder.buildTree(graph)

        networkTree.children shouldBe expectedChildren
        networkTree.parent shouldBe expectedParent
        networkTree.level shouldBe expectedLevel
        networkTree.height shouldBe 3
      }
    }
  }

  trait defaultNetworkFixture {
    val graph1 = NetworkGraph()
    val (graph2, topManager) = NetworkGraph.insertTopManager(graph1)
    val (graph3, subordinate1) = NetworkGraph.insertSubordinate(graph2, topManager)
    val (graph4, subordinate2) = NetworkGraph.insertSubordinate(graph3, topManager)
    val (graph5, subordinate3) = NetworkGraph.insertSubordinate(graph4, subordinate1)
    val (graph, subordinate4) = NetworkGraph.insertSubordinate(graph5, subordinate1)
  }

}
