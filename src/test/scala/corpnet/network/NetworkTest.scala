package corpnet.network

import org.scalatest.{FunSpec, Matchers}

class NetworkTest extends FunSpec with Matchers {

  describe("Network") {
    import Network._

    it("serializes") {
      import Network.networkProtocol._

      val network1 = Network()
      val (network2, topManager) = insertTopManager(network1)
      val (network3, subordinate1) = insertSubordinate(network2, topManager)
      val (network4, subordinate2) = insertSubordinate(network3, topManager)
      val (network, subordinate3) = insertSubordinate(network4, subordinate1)

      val serialized = toJsonString(network)
      val deserialized = fromJsonString(serialized)
      deserialized shouldBe network
    }
  }
}
