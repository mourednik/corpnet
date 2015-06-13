import breeze.linalg.DenseMatrix

import scala.collection.immutable

package object corpnet {
  type Node = Int

  type Level = Int
  type Power = Double
  type NodePower = immutable.Map[Node, Power]
  type NodeLevel = immutable.Map[Node, Level]

  type PowerLevel = Set[Level]
  type LevelNodes = immutable.Map[Level, Set[Node]]
  type NodePowerLevel = immutable.Map[Node, PowerLevel]
  type PowerLevelNodes = immutable.Map[PowerLevel, Set[Node]]

  type AdjacencyMatrix = DenseMatrix[Double]
  type Edge = (Node, Node)
}
