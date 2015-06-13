package corpnet.network

import breeze.linalg.DenseMatrix
import org.scalatest.{FunSpec, Matchers}

class NetworkGraphTest extends FunSpec with Matchers {

  import NetworkGraph._

  describe("NetworkGraph") {

    describe("nodes") {

      it("inserts a top manager (1)") {
        val (graph, topManager) = insertTopManager(NetworkGraph())
        val expected = DenseMatrix(1.0)
        assert(graph.topManagerMatrix == expected)
        assert(graph.levels(topManager) == 0)
      }

      it("inserts top manager (2)") {
        val (graph1, _) = insertTopManager(NetworkGraph())
        val (graph2, _) = insertTopManager(graph1)
        val expected = DenseMatrix((0.0, 1.0), (1.0, 0.0))
        assert(graph2.topManagerMatrix == expected)
      }

      it("inserts subordinate (1)") {
        val (graph1, topManager) = insertTopManager(NetworkGraph())
        val (graph2, subordinate) = insertSubordinate(graph1, topManager)
        val expected = DenseMatrix((0.0, 1.0), (0.0, 0.0))
        assert(graph2.subordinateMatrix == expected)
        assert(graph2.managerMatrix == expected.t)
        assert(graph2.levels(subordinate) == 1)
      }

      it("inserts subordinate (2)") {
        val (graph1, topManager) = insertTopManager(NetworkGraph())
        val (graph2, subordinate1) = insertSubordinate(graph1, topManager)
        val (graph3, subordinate2) = insertSubordinate(graph2, subordinate1)
        val expected = DenseMatrix((0.0, 1.0, 0.0), (0.0, 0.0, 1.0), (0.0, 0.0, 0.0))
        assert(graph3.subordinateMatrix == expected)
        assert(graph3.managerMatrix == expected.t)
        assert(graph3.levels(subordinate2) == 2)
      }

      it("deletes node (1)") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val graph3 = deleteNode(graph2, topManager)
        val expected = graph1.copy(nextNodeId = 1)
        assert(graph3 == expected)
        assert(graph3.levels.isEmpty)
      }

      it("deletes node (2)") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, _) = insertSubordinate(graph2, topManager)
        val graph4 = deleteNode(graph3, topManager)
        val expected = graph1.copy(nextNodeId = 2)
        assert(graph4 == expected)
        assert(graph4.levels.isEmpty)
      }

      it("deletes node (3)") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager2)
        val (graph5, subordinate2) = insertSubordinate(graph4, topManager1)
        val graph6 = deleteNode(graph5, topManager2)

        val expected = {
          val (g, m) = insertTopManager(graph1)
          val (g2, _) = insertSubordinate(g, m)
          g2
        }

        assert(graph6.topManagers.size == 1)
        assert(graph6.nodes.size == 2)
        assert(graph6.subordinateMatrix == expected.subordinateMatrix)
        assert(graph6.coworkerMatrix == expected.coworkerMatrix)
        assert(graph6.topManagerMatrix == expected.topManagerMatrix)
        assert(graph6.levels.size == 2)
      }

      it("deletes node (4)") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager1)
        val (graph4, subordinate2) = insertSubordinate(graph3, subordinate1)
        val (graph5, subordinate3) = insertSubordinate(graph4, subordinate1)
        val graph6 = deleteNode(graph5, subordinate1)
        val (graph7, subordinate1b) = insertSubordinate(graph6, topManager1)
        val (graph8, subordinate2b) = insertSubordinate(graph7, subordinate1b)
        val (graph9, subordinate3b) = insertSubordinate(graph8, subordinate1b)
        val graph10 = deleteNode(graph9, subordinate1b)

        val expected = graph2

        assert(graph10.topManagers.size == 1)
        assert(graph10.nodes.size == 1)
        assert(graph10.subordinateMatrix == expected.subordinateMatrix)
        assert(graph10.coworkerMatrix == expected.coworkerMatrix)
        assert(graph10.topManagerMatrix == expected.topManagerMatrix)
        assert(graph10.levels.size == 1)
      }

      it("bulk inserts") {
        val n = 3
        var graph = prepareForBulkInsert(NetworkGraph(), n)
        var result = bulkInsertTopManager(graph)
        var node = result._2
        graph = result._1
        for (i ← 1 until n) {
          result = bulkInsertSubordinate(graph, node)
          graph = result._1
          node = result._2
        }
        val expected = chainOfN(3)
        graph shouldBe expected
      }
    }

    describe("edges") {
      it("inserts non reporting edge") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        val (graph4, subordinate2) = insertSubordinate(graph3, topManager)
        val graph5 = insertUndirectedEdge(graph4, subordinate1, subordinate2)
        val expected = DenseMatrix((0.0, 0.0, 0.0), (0.0, 0.0, 1.0), (0.0, 1.0, 0.0))
        assert(graph5.coworkerMatrix == expected)
      }
      it("deletes non reporting edge") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        val (graph4, subordinate2) = insertSubordinate(graph3, topManager)
        val graph5 = insertUndirectedEdge(graph4, subordinate1, subordinate2)
        val graph6 = deleteUndirectedEdge(graph5, subordinate1, subordinate2)
        val expected = DenseMatrix.zeros[Double](3, 3)
        assert(graph6.coworkerMatrix == expected)
      }
      it("can not insert non-reporting edge between manager and its subordinate") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        intercept[IllegalArgumentException] {
          insertUndirectedEdge(graph3, topManager, subordinate1)
        }
      }
      it("can not insert non-reporting edge between top managers") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        intercept[IllegalArgumentException] {
          insertUndirectedEdge(graph3, topManager1, topManager2)
        }
      }
      it("clears non-reporting edges") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        val (graph4, subordinate2) = insertSubordinate(graph3, topManager)
        val (graph5, subordinate3) = insertSubordinate(graph4, topManager)
        val graph6 = insertUndirectedEdge(graph5, subordinate1, subordinate2)
        val graph7 = insertUndirectedEdge(graph6, subordinate2, subordinate3)
        val graph8 = insertUndirectedEdge(graph7, subordinate1, subordinate3)
        val graph9 = deleteAllUndirectedEdges(graph8)
        val expected = DenseMatrix.zeros[Double](4, 4)
        graph9.coworkerMatrix shouldBe expected
      }
    }

    describe("info") {
      it("finds level of a node") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        val (graph4, subordinate2) = insertSubordinate(graph3, topManager)
        val (graph5, subordinate3) = insertSubordinate(graph4, subordinate1)
        assert(findLevel(graph5, topManager) == 0)
        assert(findLevel(graph5, subordinate1) == 1)
        assert(findLevel(graph5, subordinate2) == 1)
        assert(findLevel(graph5, subordinate3) == 2)
      }

      it("finds directed edges") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager2)
        val (graph5, subordinate2) = insertSubordinate(graph4, topManager1)
        val (graph6, subordinate3) = insertSubordinate(graph5, subordinate1)
        val graph7 = insertUndirectedEdge(graph6, subordinate1, subordinate2)
        val graph8 = insertUndirectedEdge(graph7, subordinate2, subordinate3)
        val edges = findSubordinateEdges(graph8)
        val expected = Set(
          (topManager1, subordinate2),
          (topManager2, subordinate1),
          (subordinate1, subordinate3))
        assert(edges == expected)
      }

      it("finds undirected edges") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = insertSubordinate(graph4, topManager2)
        val (graph6, subordinate3) = insertSubordinate(graph5, subordinate1)
        val graph7 = insertUndirectedEdge(graph6, subordinate1, subordinate2)
        val graph8 = insertUndirectedEdge(graph7, subordinate2, subordinate3)

        val distinctEdges = findDistinctUndirectedEdges(graph8)
        val expectedDistinctEdges = Set(
          (subordinate1, subordinate2),
          (subordinate2, subordinate3))
        assert(distinctEdges == expectedDistinctEdges)

        val expectedUndirectedEdges = Set(
          (subordinate1, subordinate2),
          (subordinate2, subordinate1),
          (subordinate2, subordinate3),
          (subordinate3, subordinate2))
        findUndirectedEdges(graph8) shouldBe expectedUndirectedEdges
      }

      it("finds backflow edges") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = insertSubordinate(graph4, topManager2)
        val (graph6, subordinate3) = insertSubordinate(graph5, subordinate1)
        val graph7 = insertUndirectedEdge(graph6, subordinate1, subordinate2)
        val graph8 = insertUndirectedEdge(graph7, subordinate2, subordinate3)
        val edges = findBackflowEdges(graph8)
        val expected = Set(
          (subordinate1, topManager1),
          (subordinate2, topManager2),
          (subordinate3, subordinate1))
        assert(edges == expected)
      }

      it("finds number of coworkers and subordinates") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = insertSubordinate(graph4, subordinate1)
        val (graph6, subordinate3) = insertSubordinate(graph5, subordinate1)
        val graph7 = insertUndirectedEdge(graph6, subordinate2, subordinate3)
        val graph8 = insertUndirectedEdge(graph7, topManager2, subordinate3)

        assert(graph8.numCoworkers(topManager1) == 0)
        assert(graph8.numCoworkers(topManager2) == 1)
        assert(graph8.numCoworkers(subordinate1) == 0)
        assert(graph8.numCoworkers(subordinate2) == 1)
        assert(graph8.numCoworkers(subordinate3) == 2)
        assert(graph8.numSubordinates(topManager1) == 1)
        assert(graph8.numSubordinates(topManager2) == 0)
        assert(graph8.numSubordinates(subordinate1) == 2)
        assert(graph8.numSubordinates(subordinate2) == 0)
      }

      it("finds number of coworkers and subordinates (bulk insert)") {
        val graph1 = NetworkGraph.prepareForBulkInsert(NetworkGraph(), 5)
        val (graph2, topManager1) = bulkInsertTopManager(graph1)
        val (graph3, topManager2) = bulkInsertTopManager(graph2)
        val (graph4, subordinate1) = bulkInsertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = bulkInsertSubordinate(graph4, subordinate1)
        val (graph6, subordinate3) = bulkInsertSubordinate(graph5, subordinate1)
        val graph7 = insertUndirectedEdge(graph6, subordinate2, subordinate3)
        val graph8 = insertUndirectedEdge(graph7, topManager2, subordinate3)

        assert(graph8.numCoworkers(topManager1) == 0)
        assert(graph8.numCoworkers(topManager2) == 1)
        assert(graph8.numCoworkers(subordinate1) == 0)
        assert(graph8.numCoworkers(subordinate2) == 1)
        assert(graph8.numCoworkers(subordinate3) == 2)
        assert(graph8.numSubordinates(topManager1) == 1)
        assert(graph8.numSubordinates(topManager2) == 0)
        assert(graph8.numSubordinates(subordinate1) == 2)
        assert(graph8.numSubordinates(subordinate2) == 0)
      }

      it("finds parent") {
        val graph1 = NetworkGraph()
        val (graph2, topManager) = insertTopManager(graph1)
        val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
        val (graph4, subordinate2) = insertSubordinate(graph3, topManager)
        val (graph, subordinate3) = insertSubordinate(graph4, subordinate1)

        findParent(graph, topManager) shouldBe None
        findParent(graph, subordinate1) shouldBe Some(topManager)
        findParent(graph, subordinate2) shouldBe Some(topManager)
        findParent(graph, subordinate3) shouldBe Some(subordinate1)
      }

      it("finds all managers") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = insertSubordinate(graph4, subordinate1)
        val (graph, subordinate3) = insertSubordinate(graph5, subordinate2)

        val managers = findAllManagers(graph)
        managers shouldBe Set(topManager1, topManager2, subordinate1, subordinate2)
      }
    }
  }

  def appendChain(n: Int, graph: NetworkGraph): NetworkGraph = {
    if (graph.nodes.isEmpty)
      chainOfN(n)
    else {
      var node = graph.nodes.last
      var graph_ = graph
      (1 to n).foreach { _ ⇒
        val result = insertSubordinate(graph_, node)
        graph_ = result._1
        node = result._2
      }
      graph_
    }
  }

  def chainOfN(n: Int): NetworkGraph = {
    var (graph, node) = insertTopManager(NetworkGraph())
    appendChain(n - 1, graph)
  }
}
