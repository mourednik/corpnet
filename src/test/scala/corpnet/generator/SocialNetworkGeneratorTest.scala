package corpnet.generator

import corpnet.network.Network
import org.scalatest.{FunSpec, Matchers}

class SocialNetworkGeneratorTest extends FunSpec with Matchers {
  describe("SocialNetworkGenerator") {
    it("seems to work") {
      val network = PerfectTreeBuilder.generate(PerfectTreeParameters(3, 3, 3))
      val params = SocialNetworkParameters(0.6, 3, false)
      val network2 = Network.updateParameters(network, network.parameters.copy(delta = 10.0))
      SocialNetworkGenerator.generate(network2, params)
    }
  }
}
