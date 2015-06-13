package corpnet.network

case class NetworkParameters(
    betaMultiplier: Double = 0.9,
    delta: Double = 10.0,
    w: Double = 1.0,
    s: Double = 1.0,
    k: Double = 0.1,
    t: Double = 0.0,
    maxDepth: Int = 8,
    managerLoops: Boolean = false) {
  override def toString =
    s"NetworkParameters(betaMultiplier=$betaMultiplier, delta=$delta, w=$w, s=$s, k=$k, t=$t, maxDepth=$maxDepth)"
}

object ManagementStyle extends Enumeration {
  type ManagementStyle = Value
  val custom, autocratic, democratic, paternalistic, chaotic = Value
}

object NetworkParameters {
  def autocratic(parameters: NetworkParameters) = parameters.copy(t = 10.0, k = 0.01, delta = 5)
  def democratic(parameters: NetworkParameters) = parameters.copy(t = 0.0, k = 0.75)
  def paternalistic(parameters: NetworkParameters) = parameters.copy(t = 1.0, k = 0.3)
  def chaotic(parameters: NetworkParameters) = parameters.copy(t = 0.0, k = 1.0, w = 2.0)
}
