package corpnet.generator

import corpnet._
import corpnet.network.{Network, NetworkParameters}

import scala.collection.mutable

case class PerfectTreeParameters(
  height: Int = 4,
  childrenPerNode: Int = 3,
  freeCapacity: Int = 3,
  w: Double = 0.5,
  k: Double = 0.1,
  t: Double = 0.1)

object PerfectTreeBuilder {

  def generate(params: PerfectTreeParameters): Network = {
    val n = numNodes(params.childrenPerNode, params.height - 1)
    if (n <= 0)
      throw new RuntimeException(s"Tree would have too few nodes ($n)")
    if (n > 10000)
      throw new RuntimeException(s"Tree would have too many nodes ($n)")
    val queue = mutable.Queue.empty[(Node, Int)]
    val delta = params.childrenPerNode + 1 + params.freeCapacity
    val parameters = NetworkParameters().copy(
      delta = delta, maxDepth = params.height, w = params.w, k = params.k, t = params.t)
    val initialNetwork = Network(parameters)
    var (network, topManager) = Network.insertTopManager(initialNetwork)
    network = Network.prepareForBulkInsert(network, n - 1)
    queue.enqueue((topManager, 1))
    while (queue.nonEmpty) {
      val (node, level) = queue.dequeue()
      if (level < params.height) {
        for (_ ← 1 to params.childrenPerNode) {
          Network.bulkInsertSubordinate(network, node) match {
            case (network_, currentNode) ⇒
              network = network_
              queue.enqueue((currentNode, level + 1))
          }
        }
      }
    }
    network
  }

  def numNodes(k: Int, height: Int): Int = {
    Math.round((Math.pow(k, height + 1) - 1.0) / (k - 1.0)).toInt
  }
}
