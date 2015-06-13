package corpnet.layout

import corpnet.layout.Force._
import org.scalatest.{FunSpec, Matchers}

class ForceTest extends FunSpec with Matchers {

  describe("Forces") {
    implicit val fc = ForceConstants(1.0, 1.0, 1.0, 1.0)

    it("third law") {
      new Fixture {
        val force1 = Force.adjacent(node1, node4)
        val force2 = Force.adjacent(node4, node1)

        force1.magnitude shouldBe force2.magnitude
        force1.xUnit shouldBe (-force2.xUnit)
        force1.yUnit shouldBe (-force2.yUnit)

        norm(force1.xUnit, force1.yUnit) shouldBe 1.0 +- 1E-9
      }
    }

    it("sums opposite forces") {
      new Fixture {
        val force1 = Force.adjacent(node1, node4)
        val force2 = Force.adjacent(node4, node1)
        val force3 = force1 + force2
        force3.magnitude shouldBe 0.0
        force3.xUnit shouldBe 0.0
        force3.yUnit shouldBe 0.0
      }
    }

    it("sums orthogonal forces") {
      new Fixture {
        val force12 = Force.adjacent(node1, node2)
        val force42 = Force.adjacent(node4, node2)
        val sumForces = force12 + force42
        sumForces.magnitude * sumForces.yUnit shouldBe force12.magnitude * force12.yUnit
        sumForces.magnitude * sumForces.xUnit shouldBe force42.magnitude * force42.xUnit
      }
    }

    describe("Adjacent force acts like a spring") {

      describe("infinitely compressed") {
        new Fixture {
          val force = Force.adjacent(node1, node1)
          force.magnitude shouldBe Force.MAX_MAGNITUDE
        }
      }

      it("compressed") {
        new Fixture {
          override lazy val distance = 0.5
          val force = Force.adjacent(node1, node2)
          force.magnitude shouldBe >(0.0)
          force.magnitude shouldBe <(Force.MAX_MAGNITUDE)
          force.yUnit shouldBe >(0.0)
          force.xUnit shouldBe 0.0
        }
      }

      it("relaxed") {
        new Fixture {
          override lazy val distance = 1.0
          val force = Force.adjacent(node1, node2)
          force shouldBe Force(0.0, 1.0, 0.0)
        }
      }

      it("extended") {
        new Fixture {
          val force = Force.adjacent(node1, node2)
          force.magnitude shouldBe >(0.0)
          force.magnitude shouldBe <(Force.MAX_MAGNITUDE)
          force.yUnit shouldBe <(0.0)
          force.xUnit shouldBe 0.0
        }
      }
    }

    describe("Non adjacent force is repellent") {

      describe("singularity") {
        new Fixture {
          val force = Force.nonAdjacent(node1, node1)
          force.magnitude shouldBe Force.MAX_MAGNITUDE
        }
      }

      describe("near") {
        new Fixture {
          override lazy val distance = 0.1
          val force = Force.nonAdjacent(node1, node2)
          force.magnitude shouldBe >(0.0)
          force.magnitude shouldBe <(Force.MAX_MAGNITUDE)
          force.yUnit shouldBe >(0.0)
          force.xUnit shouldBe 0.0
        }
      }

      describe("far") {
        new Fixture {
          override lazy val distance = 1E10
          val force = Force.nonAdjacent(node1, node2)
          force.magnitude shouldBe >(0.0)
          force.magnitude shouldBe <(Force.MAX_MAGNITUDE)
          force.yUnit shouldBe >(0.0)
          force.xUnit shouldBe 0.0
        }
      }
    }
  }

  describe("Positions") {

    it("can be shifted by forces (1)") {
      val force = Force(1.0, 1.0)
      val position = Position(0.0, 0.0)
      val finalPosition = position * (force, 0.1)
      finalPosition shouldBe Position(0.1, 0.1)
    }

    it("can be shifted by forces (2)") {
      val force = Force(1.0, 0.0)
      val position = Position(0.0, 0.0)
      val finalPosition = position * (force, 0.5)
      finalPosition shouldBe Position(0.5, 0.0)
    }
  }

  trait Fixture {
    lazy val distance = 2.0
    lazy val node1 = Position(0.0, 0.0)
    lazy val node2 = Position(0.0, distance)
    lazy val node3 = Position(distance, 0.0)
    lazy val node4 = Position(distance, distance)
  }

}
