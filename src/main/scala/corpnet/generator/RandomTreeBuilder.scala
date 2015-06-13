package corpnet.generator

import corpnet._
import corpnet.network.{ NetworkParameters, Network }
import probability_monad.Distribution

import scala.annotation.tailrec
import scala.collection.mutable

case class RandomTreeParameters(
  initialMean: Double = 3.0,
  standardDeviation: Double = 1.0,
  meanIncrementPerLevel: Double = 0.5,
  height: Int = 4,
  freeCapacity: Double = 8.0,
  w: Double = 0.5,
  k: Double = 0.1,
  t: Double = 0.1)

object RandomTreeBuilder {

  def generateFamilySizes(params: RandomTreeParameters): Map[Int, List[Int]] = {

    def chooseFamilySizes(n: Int, mean: Double, standardDeviation: Double): List[Int] =
      Distribution.normal.sample(n)
        .map(_ * standardDeviation + mean)
        .map(Math.max(_, 0.0))
        .map(Math.round)
        .map(_.toInt)

    case class State(numNodes: Int, level: Int, families: Map[Int, List[Int]])

    def nextLevel(state: State): State = {
      val mean = params.initialMean + state.level * params.meanIncrementPerLevel
      val familySizes = chooseFamilySizes(state.numNodes, mean, params.standardDeviation)
      val families = state.families ++ Map(state.level -> familySizes)
      State(familySizes.sum, state.level + 1, families)
    }

    @tailrec
    def go(state: State): State =
      if (state.level == params.height - 1)
        state
      else
        go(nextLevel(state))

    val initialState = State(1, 0, Map.empty)
    val finalState = go(initialState)
    finalState.families
  }

  def treeSize(familySizes: Map[Int, List[Int]]): Int =
    familySizes.values.map(_.sum).sum + 1

  def maxFamilySize(familySizes: Map[Int, List[Int]]): Int =
    familySizes.values.map(_.max).max

  def generate(params: RandomTreeParameters): Network = {
    val familySizes = generateFamilySizes(params)

    val n = treeSize(familySizes)
    if (n <= 0)
      throw new RuntimeException(s"Tree would have too few nodes ($n)")
    if (n > 10000)
      throw new RuntimeException(s"Tree would have too many nodes ($n)")

    val maxUsedCapacity = maxFamilySize(familySizes)

    val delta = maxUsedCapacity + 1 + params.freeCapacity
    val parameters = NetworkParameters().copy(
      delta = delta, maxDepth = params.height, w = params.w, k = params.k, t = params.t)
    var (network, topManager) = Network.insertTopManager(Network(parameters))
    network = Network.prepareForBulkInsert(network, n - 1)

    val mutableFamilySizes = mutable.Map.empty ++ familySizes
    val queue = mutable.Queue.empty[(Node, Int)]
    queue.enqueue((topManager, 0))

    def popNextFamilySize(level: Int): Option[Int] = {
      val result = mutableFamilySizes(level).headOption
      mutableFamilySizes(level) = mutableFamilySizes(level) match {
        case Nil    ⇒ Nil
        case h :: t ⇒ t
      }
      result
    }

    while (queue.nonEmpty) {
      val (node, level) = queue.dequeue()
      if (level < params.height - 1) {
        for {
          numChildren ← popNextFamilySize(level)
          _ ← 1 to numChildren
        } {
          Network.bulkInsertSubordinate(network, node) match {
            case (nextNetwork, currentNode) ⇒
              network = nextNetwork
              queue.enqueue((currentNode, level + 1))
          }
        }
      }
    }
    network
  }
}
