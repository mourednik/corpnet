package corpnet.power

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import corpnet.network.{NetworkGraph, NetworkParameters}
import org.scalatest.{FunSpec, Matchers}

class BonacichPowerTest extends FunSpec with Matchers {

  import BonacichPower._
  import NetworkGraph._

  describe("Bonacich Power (Exact)") {

    it("two top managers (t=0.0)") {
      new DefaultParametersFixture {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = NetworkGraph.insertTopManager(graph1)
        val (graph3, topManager2) = NetworkGraph.insertTopManager(graph2)

        val bonacichPower = calculate(graph3, parameters, ComputationMethod.Exact)
        bonacichPower.isDefined shouldBe false
      }
    }

    it("two top managers (t=0.1)") {
      new DefaultParametersFixture {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = NetworkGraph.insertTopManager(graph1)
        val (graph, topManager2) = NetworkGraph.insertTopManager(graph2)

        override val t = 0.1

        val expectedBeta = 9.94999985173345
        val expectedPower = DenseVector(20.0, 20.0)
        val expectedNormalizedPower = DenseVector(1.0, 1.0)

        val relationMatrix = buildRelationMatrix(graph, parameters)
        val beta = calculateBetaWithEigenvalue(relationMatrix, parameters.betaMultiplier)
        norm(beta - expectedBeta) < 1E-6 shouldBe true

        val power = calculateVectorExactly(relationMatrix, beta)
        norm(power - expectedPower) < 1E-2 shouldBe true

        val normalizedPower = normalize(power)
        norm(normalizedPower - expectedNormalizedPower) < 1E-3 shouldBe true
      }
    }

    it("chain of five (t=0.0)") {
      new chainOfFiveFixture with DefaultParametersFixture {
        val expectedBeta = 0.869662
        val expectedPower = DenseVector(295.072, 338.146, 239.563, 104.669)
        val expectedNormalizedPower = DenseVector(1.136, 1.302, 0.922, 0.403)

        val relationMatrix = buildRelationMatrix(graph, parameters)
        val beta = calculateBetaWithEigenvalue(relationMatrix, parameters.betaMultiplier)
        norm(beta - expectedBeta) < 1.2E-7 shouldBe true

        val power = calculateVectorExactly(relationMatrix, beta)
        norm(power - expectedPower) < 0.003 shouldBe true

        val normalizedPower = normalize(power)
        norm(normalizedPower - expectedNormalizedPower) < 5.7E-4 shouldBe true
      }
    }

    it("chain of five (t=0.1)") {
      new chainOfFiveFixture with DefaultParametersFixture {
        override val t = 0.1

        val expectedBeta = 0.858406762403642
        val expectedPower = DenseVector(321.138, 340.714, 234.598, 101.190)
        val expectedNormalizedPower = DenseVector(1.204, 1.278, 0.880, 0.379)

        val relationMatrix = buildRelationMatrix(graph, parameters)
        val beta = calculateBetaWithEigenvalue(relationMatrix, parameters.betaMultiplier)
        norm(beta - expectedBeta) < 1.2E-7 shouldBe true

        val power = calculateVectorExactly(relationMatrix, beta)
        norm(power - expectedPower) < 0.003 shouldBe true

        val normalizedPower = normalize(power)
        norm(normalizedPower - expectedNormalizedPower) < 1E-3 shouldBe true
      }
    }
  }

  describe("Bonacich Power") {

    it("builds adjacency list representation of adjacency matrix") {
      val matrix = DenseMatrix(
        (0.0, 1.0, 0.0),
        (0.1, 0.0, 0.9),
        (0.0, 0.0, 0.0))
      val expected = Map(0 -> List(1), 1 -> List(0, 2), 2 -> List())
      buildAdjacencyList(matrix) shouldBe expected
    }

    ignore("benchmark") {
      new DefaultParametersFixture {
        override val k = 0.1
        override val delta = 2

        def benchmark(graph: NetworkGraph) = {
          val n = graph.nodes.size
          val startTime = System.currentTimeMillis()
          val exactResult = calculate(graph, parameters, ComputationMethod.Exact)
          val endTime = System.currentTimeMillis()
          val totalTime = (endTime - startTime) / 1000.0
          println(s"$n,$totalTime")
        }

        var graph = NetworkGraph()
        (1 to 25).foreach { _ ⇒
          graph = appendChain(100, graph)
          benchmark(graph)
        }
      }
    }

    describe("data structures") {
      it("builds loop matrix") {
        val graph1 = NetworkGraph()
        val (graph2, topManager1) = insertTopManager(graph1)
        val (graph3, topManager2) = insertTopManager(graph2)
        val (graph4, subordinate1) = insertSubordinate(graph3, topManager1)
        val (graph5, subordinate2) = insertSubordinate(graph4, subordinate1)
        val (graph6, subordinate3) = insertSubordinate(graph5, subordinate2)
        val graph = insertUndirectedEdge(graph6, topManager2, subordinate2)

        val params = NetworkParameters(w = 0.5)
        val loopMatrix = buildLoopMatrix(graph, params)
        val c = params.delta
        val expected = DenseMatrix(
          ((c - 1) / c, 0.0, 0.0, 0.0, 0.0),
          (0.0, (c - 0.5) / c, 0.0, 0.0, 0.0),
          (0.0, 0.0, (c - 2) / c, 0.0, 0.0),
          (0.0, 0.0, 0.0, (c - 2.5) / c, 0.0),
          (0.0, 0.0, 0.0, 0.0, 0.0))
        loopMatrix shouldBe expected
      }
    }
  }

  trait chainOfFiveFixture {
    val graph1 = NetworkGraph()
    val (graph2, topManager) = insertTopManager(graph1)
    val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
    val (graph4, subordinate2) = insertSubordinate(graph3, subordinate1)
    val (graph, subordinate3) = insertSubordinate(graph4, subordinate2)
  }

  trait chainOfNineFixture {
    val graph1 = NetworkGraph()
    val (graph2, topManager) = insertTopManager(graph1)
    val (graph3, subordinate1) = insertSubordinate(graph2, topManager)
    val (graph4, subordinate2) = insertSubordinate(graph3, subordinate1)
    val (graph5, subordinate3) = insertSubordinate(graph4, subordinate2)
    val (graph6, subordinate4) = insertSubordinate(graph4, subordinate3)
    val (graph7, subordinate5) = insertSubordinate(graph4, subordinate4)
    val (graph8, subordinate6) = insertSubordinate(graph4, subordinate5)
    val (graph9, subordinate7) = insertSubordinate(graph4, subordinate6)
    val (graph, subordinate8) = insertSubordinate(graph4, subordinate7)
  }

  def appendChain(n: Int, graph: NetworkGraph): NetworkGraph = {
    if (graph.nodes.isEmpty)
      chainOfN(n)
    else {
      var node = graph.nodes.last
      var graph_ = prepareForBulkInsert(graph, n)
      (1 to n).foreach { _ ⇒
        val result = bulkInsertSubordinate(graph_, node)
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

  trait DefaultParametersFixture {
    val betaMultiplier = 0.995
    val delta = 10
    val w = 1.0
    val s = 1.0
    val k = 0.5
    val t = 0.0
    val maxDepth = 4
    val managerLoops = false
    lazy val parameters = NetworkParameters(betaMultiplier = betaMultiplier, delta = delta, w = w, s = s, k = k, t = t,
      maxDepth = maxDepth, managerLoops = managerLoops)
  }

}
