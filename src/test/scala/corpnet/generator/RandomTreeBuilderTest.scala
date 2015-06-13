package corpnet.generator

import org.scalatest.{FunSpec, Matchers}

class RandomTreeBuilderTest extends FunSpec with Matchers {
  describe("Random Tree Builder") {
    it("seems to work") {
      val params = RandomTreeParameters(3.0, 0.7, 0.7, 3, 1.0, 0.5, 0.1, 0.1)
      val result = RandomTreeBuilder.generate(params)
    }
  }
}
