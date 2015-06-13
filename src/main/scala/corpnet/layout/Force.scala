package corpnet.layout

case class Force(xUnit: Double, yUnit: Double, magnitude: Double) {

  def +(other: Force)(implicit fc: ForceConstants): Force = {
    val xComponent = magnitude * xUnit + other.magnitude * other.xUnit
    val yComponent = magnitude * yUnit + other.magnitude * other.yUnit
    Force(xComponent, yComponent)
  }
}

object Force {

  val MAX_MAGNITUDE = 100.0

  def apply(xComponent: Double, yComponent: Double): Force = {
    val length = norm(xComponent, yComponent)
    if (length > 0.0) {
      Force(xComponent / length, yComponent / length, length) // more accurate than component * 1.0/length
    } else {
      Force(0.0, 0.0, 0.0)
    }
  }

  def adjacentMagnitude(distance: Double)(implicit fc: ForceConstants): Double =
    Math.min(-fc.c1 * Math.log(distance / fc.c2), MAX_MAGNITUDE)

  def nonAdjacentMagnitude(distance: Double)(implicit fc: ForceConstants): Double =
    Math.min(fc.c3 / (distance * distance), MAX_MAGNITUDE)

  def calculateForce(
    node1: Position, node2: Position, magnitudeFunc: Double ⇒ Double)(implicit fc: ForceConstants): Force = {
    val xDistance = node2.x - node1.x
    val yDistance = node2.y - node1.y
    val length = norm(xDistance, yDistance)
    val magnitude = magnitudeFunc(length)
    val sign = if (magnitude < 0.0) -1.0 else 1.0
    val absMagnitude = sign * magnitude
    apply(sign * xDistance / length, sign * yDistance / length, absMagnitude)
  }

  def adjacent(node1: Position, node2: Position)(implicit fc: ForceConstants): Force =
    calculateForce(node1, node2, adjacentMagnitude)

  def nonAdjacent(node1: Position, node2: Position)(implicit fc: ForceConstants): Force =
    calculateForce(node1, node2, nonAdjacentMagnitude)

  def norm(x: Double, y: Double) =
    Math.sqrt(x * x + y * y)
}
