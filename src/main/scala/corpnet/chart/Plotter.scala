package corpnet.chart

import breeze.plot._
import corpnet.{NodeLevel, NodePower}

object Plotter {

  def plotPowers(nodePower: NodePower, nodeLevel: NodeLevel) = {
    val levelPower = nodePower.toList.map {
      case (node, power) => (nodeLevel(node), power)
    }
    val sortedPowers = levelPower
      .groupBy(_._1) // map of level -> list of (level, power)
      .mapValues(_.map(_._2) // map of level -> list of powers
      .sortWith((a, b) => a >= b)) // map of level -> descending list of powers
      .toList.sortBy(_._1) // list of (level, list(powers)) by ascending level
      .flatMap(_._2) // descending powers grouped by level

    val f = Figure()
    val p = f.subplot(0)
    val x = sortedPowers.indices.map(_.toDouble)
    val y = sortedPowers
    p += plot(x, y, '+')
    p.xlabel = "node"
    p.ylabel = "power"
    p.title = "Power"
    f.visible = true
  }

  def plotHistogram(nodePower: NodePower, bins: Int) = {
    val f = Figure()
    val p = f.subplot(0)
    p += hist(nodePower.values.toList, bins)
    p.title = "Power Histogram"
    f.visible = true
  }
}
