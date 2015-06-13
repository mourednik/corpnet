package corpnet.power

import corpnet._
import corpnet.generator.NetworkTree

/**
  * A node A is stable if for all nodes B such that level(A) < level(B), the power of A is greater than the power of B,
  * i.e. p(A) > p(B).
  *
  * A is strongly unstable if there exists a node B such that B is one of the children of A, and p(A) <= p(B).
  *
  * Finally, it is weakly unstable if it is not stable nor strongly unstable.
  *
  * Then, all unstable nodes = strongly unstable + weakly unstable.
  */
object NetworkStability extends Enumeration {
  val StronglyStable, WeaklyStable, Unstable = Value
}

case class LevelStats(level: Level, min: Double, max: Double, avg: Double) {
  override def toString = f"$level, $min%2.4f, $max%2.4f, $avg%2.4f"
}

object LevelStats {
  def apply(level: Level, powerValues: Iterable[Power]): LevelStats =
    LevelStats(level, powerValues.min, powerValues.max, powerValues.sum / powerValues.size)
}

case class PowerAnalysis(
    weaklyUnstableNodes: Set[Node],
    stronglyUnstableNodes: Set[Node],
    weaklyUnstableRatio: Double,
    stronglyUnstableRatio: Double,
    unstableRatio: Double,
    unstableLevels: Set[Level],
    levelStats: List[LevelStats]) {

  override def toString = {
    val unstableNodes = stronglyUnstableNodes ++ weaklyUnstableNodes
    val header =
      if (unstableNodes.isEmpty) "Network is stable"
      else s"Network contains unstable nodes"
    val lines = List(
      Some(header),
      Option(unstableNodes).filter(_.nonEmpty).map(_ ⇒
        f"  Unstable ratio = $unstableRatio%2.4f, total=${unstableNodes.size}"),
      Option(stronglyUnstableNodes).filter(_.nonEmpty).map(_ ⇒
        f"  Strongly unstable ratio = $stronglyUnstableRatio%2.4f, total=${stronglyUnstableNodes.size}"),
      Option(weaklyUnstableNodes).filter(_.nonEmpty).map(_ ⇒
        f"  Weakly unstable ratio = $weaklyUnstableRatio%2.4f, total=${weaklyUnstableNodes.size}"),
      Option(unstableLevels).filter(_.nonEmpty).map(_ ⇒
        s"  Unstable levels: ${unstableLevels.mkString(", ")}"))
    lines.flatten.mkString("\n") + "\n" + PowerAnalysis.levelStatsString(levelStats)
  }
}

object PowerAnalysis {

  def levelStatsString(levelStats: List[LevelStats]): String = {
    val header = "  level, min, max, avg\n"
    val lines = levelStats.sortBy(_.level).map("  " + _.toString)
    header + lines.mkString("\n")
  }

  def calculateLevelStats(nodePower: NodePower, nodeLevel: NodeLevel): List[LevelStats] = {
    val levelToNodes = nodeLevel.groupBy(_._2).mapValues(l ⇒ l.keys)
    val levelToPowers = levelToNodes.mapValues(_.map(nodePower))
    levelToPowers.map { case (level, powerValues) ⇒ level -> LevelStats(level, powerValues) }.values.toList
  }

  def findWeaklyUnstableNodes(unstableNodes: Set[Node], stronglyUnstableNodes: Set[Node]): Set[Node] =
    unstableNodes.diff(stronglyUnstableNodes)

  def findUnstableNodes(powers: NodePower, levels: NodeLevel): Set[Node] = {
    case class NodePowerLevel(node: Node, power: Power, level: Level)

    def lessThan(npl1: NodePowerLevel, npl2: NodePowerLevel) =
      (npl1.power > npl2.power) || (npl1.power == npl2.power && npl1.level > npl2.level)

    val nodePowersLevels = powers.map { case (node, power) ⇒ NodePowerLevel(node, power, levels(node)) }
    val sorted = nodePowersLevels.toSeq.sortWith(lessThan)

    val (_, unstableNodes) = sorted.foldLeft((0, Set.empty[Node])) {
      case ((deepest, unstableNodes), NodePowerLevel(node, power, level)) if level > deepest ⇒
        (level, unstableNodes)
      case ((deepest, unstableNodes), NodePowerLevel(node, power, level)) if level < deepest ⇒
        (deepest, unstableNodes + node)
      case ((deepest, unstableNodes), NodePowerLevel(node, power, level)) ⇒
        (deepest, unstableNodes)
    }

    unstableNodes
  }

  def findStronglyUnstableNodes(powers: NodePower, tree: NetworkTree): Set[Node] = {

    def findUnstable(chain: List[(Node, Power)]) = {
      def go(chain1: List[(Node, Power)], max: Power, unstableNodes: List[Node]): List[Node] =
        chain1 match {
          case (node, power) :: t if power > max  ⇒ go(t, power, unstableNodes)
          case (node, power) :: t if power <= max ⇒ go(t, max, node :: unstableNodes)
          case Nil                                ⇒ unstableNodes
        }
      go(chain, 0.0, Nil)
    }

    val leafNodes = NetworkTree.leafNodes(tree)
    val chains = for {
      leaf ← leafNodes
    } yield {
      val chain = NetworkTree.chain(leaf, tree)
      val chainWithPowers = chain.map(node ⇒ (node, powers(node)))
      findUnstable(chainWithPowers.reverse)
    }
    chains.flatten.toSet
  }

  def analyze(nodePower: NodePower, networkTree: NetworkTree): PowerAnalysis = {
    val numNodes = nodePower.size
    val nodeLevel = networkTree.level
    val unstableNodes = findUnstableNodes(nodePower, nodeLevel)
    val stronglyUnstableNodes = findStronglyUnstableNodes(nodePower, networkTree)
    val weaklyUnstableNodes = unstableNodes.diff(stronglyUnstableNodes)
    val weaklyUnstableRatio = weaklyUnstableNodes.size.toDouble / numNodes
    val stronglyUnstableRatio = stronglyUnstableNodes.size.toDouble / numNodes
    val unstableRatio = unstableNodes.size.toDouble / numNodes
    val unstableLevels = nodeLevel.filter(nodeLevel ⇒ unstableNodes.contains(nodeLevel._1)).values.toSet
    val levelStats = calculateLevelStats(nodePower, nodeLevel)

    PowerAnalysis(weaklyUnstableNodes, stronglyUnstableNodes, weaklyUnstableRatio,
      stronglyUnstableRatio, unstableRatio, unstableLevels, levelStats)
  }
}
