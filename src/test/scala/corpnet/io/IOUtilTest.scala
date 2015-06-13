package corpnet.io

import corpnet.network.NetworkGraph
import org.scalatest.{FunSpec, Matchers}

class IOUtilTest extends FunSpec with Matchers {
  describe("TSV") {
    it("generates TSV string") {
      new DefaultGraphFixture {
        val expected =
          """
            |0 2 1.0
            |1 3 1.0
            |1 4 1.0
            |2 0 1.0
            |2 3 1.0
            |3 1 1.0
            |3 2 1.0
            |4 1 1.0
          """.stripMargin.trim.replaceAll(" ", "\t")
        val result = IOUtil.edgesToTSV(NetworkGraph.findUndirectedEdges(graph))
        result shouldBe expected
      }
    }
  }

  trait DefaultGraphFixture {

    import NetworkGraph._

    val (graph1, node0) = insertTopManager(NetworkGraph())
    val (graph2, node1) = insertSubordinate(graph1, node0)
    val (graph3, node2) = insertSubordinate(graph2, node1)
    val (graph4, node3) = insertSubordinate(graph3, node0)
    val (graph5, node4) = insertSubordinate(graph4, node3)

    val graph6 = insertUndirectedEdge(graph5, node1, node3)
    val graph7 = insertUndirectedEdge(graph6, node1, node4)
    val graph8 = insertUndirectedEdge(graph7, node2, node3)
    val graph = insertUndirectedEdge(graph8, node2, node0)
  }
}
