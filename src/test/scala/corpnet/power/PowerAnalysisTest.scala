package corpnet.power

import corpnet.generator.NetworkTreeBuilder
import corpnet.network.NetworkGraph
import corpnet.{NodeLevel, NodePower}
import org.scalatest.{Matchers, WordSpec}

class PowerAnalysisTest extends WordSpec with Matchers {

  import PowerAnalysis._

  "PowerAnalysis" should {
    "find unstable nodes" in new WeaklyUnstableFixture {
      findUnstableNodes(powers, levels) shouldBe Set(sub3)
      findStronglyUnstableNodes(powers, tree) shouldBe Set.empty
    }
    "finds strongly unstable nodes" in new StronglyUnstableFixture {
      findUnstableNodes(powers, levels) shouldBe Set(top, sub1, sub3)
      findStronglyUnstableNodes(powers, tree) shouldBe Set(top, sub1, sub3)
    }
    "generate a report (1)" in new WeaklyUnstableFixture {
      val analysis = analyze(powers, tree)
      analysis.weaklyUnstableNodes shouldBe Set(sub3)
      analysis.stronglyUnstableNodes shouldBe Set.empty
      analysis.weaklyUnstableRatio shouldBe 1.0 / 5.0
      analysis.stronglyUnstableRatio shouldBe 0.0
      analysis.unstableRatio shouldBe 1.0 / 5.0
      analysis.unstableLevels shouldBe Set(1)
    }
    "generate a report (2)" in new StronglyUnstableFixture {
      val analysis = analyze(powers, tree)
      analysis.weaklyUnstableNodes shouldBe Set.empty
      analysis.stronglyUnstableNodes shouldBe Set(top, sub1, sub3)
      analysis.weaklyUnstableRatio shouldBe 0.0
      analysis.stronglyUnstableRatio shouldBe 3.0 / 5.0
      analysis.unstableRatio shouldBe 3.0 / 5.0
      analysis.unstableLevels shouldBe Set(0, 1)
    }
  }

  trait Fixture {

    import NetworkGraph._

    val (graph1, top) = insertTopManager(NetworkGraph())
    val (graph2, sub1) = insertSubordinate(graph1, top)
    val (graph3, sub2) = insertSubordinate(graph2, sub1)
    val (graph4, sub3) = insertSubordinate(graph3, top)
    val (graph, sub4) = insertSubordinate(graph4, sub3)

    val tree = NetworkTreeBuilder.buildTree(graph)

    val levels: NodeLevel = Map(
      top -> 0,
      sub1 -> 1, sub3 -> 1,
      sub2 -> 2, sub4 -> 2)
  }

  trait WeaklyUnstableFixture extends Fixture {
    val powers: NodePower = Map(
      top -> 1.0,
      sub1 -> 0.9, sub3 -> 0.8,
      sub2 -> 0.8, sub4 -> 0.6)
  }

  trait StronglyUnstableFixture extends Fixture {
    val powers: NodePower = Map(
      top -> 1.0,
      sub1 -> 0.9, sub3 -> 0.9,
      sub2 -> 1.0, sub4 -> 1.0)
  }
}
