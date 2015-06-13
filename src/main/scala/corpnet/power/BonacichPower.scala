package corpnet.power

import breeze.linalg._
import corpnet._
import corpnet.network.{NetworkGraph, NetworkParameters}
import corpnet.power.BonacichPower.ComputationMethod.ComputationMethod

case class BonacichPower(
    nodePower: NodePower,
    beta: Double,
    betaMultiplier: Double,
    betaMethod: BonacichPower.BetaMethod.Value,
    computationMethod: BonacichPower.ComputationMethod.Value,
    iterations: Option[Int] = None) {

  override def toString = {
    val iterationsStr = iterations.map(iters ⇒ s", iterations=$iters").getOrElse("")
    s"BonacichPower(beta=$beta, betaMultiplier=$betaMultiplier, betaMethod=$betaMethod, " +
      s"computationMethod=$computationMethod$iterationsStr)"
  }
}

object BonacichPower {

  object BetaMethod extends Enumeration {
    type BetaMethod = Value
    val MaxEigenvalue, Capacity = Value
  }

  object ComputationMethod extends Enumeration {
    type ComputationMethod = Value
    val Exact, Iterative = Value
  }

  def buildLoopMatrix(graph: NetworkGraph, parameters: NetworkParameters): DenseMatrix[Double] = {
    val n = graph.nodes.size
    val loopMatrix = DenseMatrix.zeros[Double](n, n)
    for {
      manager ← NetworkGraph.findAllManagers(graph).toList
    } {
      val numCoworkers = graph.numCoworkers(manager)
      val numSubordinates = graph.numSubordinates(manager)
      val isTopManager = graph.topManagers.contains(manager)
      val capacityUsedByManager = if (isTopManager) 0 else 1
      val usedCapacity = capacityUsedByManager + numCoworkers * parameters.w + numSubordinates * parameters.s // + numCoManagers * parameters.t
      val remainingCapacity = parameters.delta - usedCapacity
      val index = NetworkGraph.findIndex(graph, manager)
      loopMatrix(index, index) = remainingCapacity / parameters.delta
    }
    loopMatrix
  }

  def buildRelationMatrix(graph: NetworkGraph, parameters: NetworkParameters): DenseMatrix[Double] = {
    val partialMatrix = graph.subordinateMatrix + parameters.k * (graph.coworkerMatrix + graph.managerMatrix)
    parameters.managerLoops match {
      case true if graph.topManagers.size == 1 ⇒
        val loopMatrix = buildLoopMatrix(graph, parameters)
        partialMatrix + loopMatrix // ignore topManagerMatrix single loop
      case true if graph.topManagers.size != 1 ⇒
        val loopMatrix = buildLoopMatrix(graph, parameters)
        partialMatrix + loopMatrix + parameters.t * graph.topManagerMatrix
      case false ⇒
        partialMatrix + parameters.t * graph.topManagerMatrix
    }
  }

  def neighbours(index: Int, matrix: DenseMatrix[Double]): List[Int] = {
    val row = matrix(index, ::)
    val nonZero = row.inner :> 0.0
    nonZero.activeKeysIterator.toList
  }

  def buildAdjacencyList(matrix: DenseMatrix[Double]): Map[Int, List[Int]] =
    (0 until matrix.rows).map(i ⇒ i -> neighbours(i, matrix)).toMap

  def calculateLambda(relationMatrix: AdjacencyMatrix): Double =
    max(eig(relationMatrix).eigenvalues)

  def calculateBetaWithEigenvalue(relationMatrix: AdjacencyMatrix, betaMultiplier: Double): Double =
    betaMultiplier / calculateLambda(relationMatrix)

  def calculateBetaWithCapacity(parameters: NetworkParameters): Double = {
    val relativeCapacity = parameters.delta / parameters.s
    parameters.betaMultiplier / (relativeCapacity - 1.0)
  }

  def calculateVectorExactly(relationMatrix: AdjacencyMatrix, beta: Double): DenseVector[Double] = {
    val n = relationMatrix.rows
    val identity = DenseMatrix.eye[Double](n)
    val ones = DenseVector.ones[Double](n)
    (identity - beta * relationMatrix) \ (relationMatrix * ones)
  }

  def normalizingFactor(power: DenseVector[Double]): Double = {
    val n = power.length
    math.sqrt(n) / norm(power) // normalized such that |power|^2 = n
  }

  def normalize(power: DenseVector[Double]): DenseVector[Double] = {
    power * normalizingFactor(power)
  }

  def isNonZeroGraph(graph: NetworkGraph, parameters: NetworkParameters): Boolean = {
    def isOnlyTopManagers(graph: NetworkGraph) = graph.topManagers.size == graph.nodes.size
    !(parameters.t == 0.0 && isOnlyTopManagers(graph))
  }

  def calculate(graph: NetworkGraph, parameters: NetworkParameters, method: ComputationMethod): Option[BonacichPower] =
    if (isNonZeroGraph(graph, parameters)) {
      val relationMatrix = buildRelationMatrix(graph, parameters)
      val beta1 = calculateBetaWithCapacity(parameters)
      val beta2 = calculateBetaWithEigenvalue(relationMatrix, parameters.betaMultiplier)
      val (beta, betaMethod) =
        if (beta1 < beta2)
          (beta1, BetaMethod.Capacity)
        else
          (beta2, BetaMethod.MaxEigenvalue)

      val (powers, iterations) = method match {
        case ComputationMethod.Exact ⇒
          val powers = normalize(calculateVectorExactly(relationMatrix, beta))
          (powers, None)
        case ComputationMethod.Iterative ⇒
          throw new Exception("Iterative method not implemented")
      }
      val normalizedPowers = normalize(powers)
      val nodePower: NodePower = (graph.nodes zip normalizedPowers.toScalaVector).toMap
      Some(BonacichPower(nodePower, beta, parameters.betaMultiplier, betaMethod, method, iterations))
    } else None
}
