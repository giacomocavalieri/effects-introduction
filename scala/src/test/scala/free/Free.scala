package free

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import free.core.maybeDouble
import free.interpreters.Test.*

// With the test interpreter testing the core logic is a breeze!
class Test extends AnyWordSpec, Matchers:
  val program = maybeDouble(10)
  "maybeDouble" when {
    "executed" should {
      "always print a message regardless of the coin flip result" in {
        val (msg1, _) = program.runWithRiggedCoin(true)
        val (msg2, _) = program.runWithRiggedCoin(false)
        msg1 shouldBe "Flipping a coin!"
        msg2 shouldBe "Flipping a coin!"
      }
    }
    "the coin flip result is heads" should {
      "double the number" in {
        val (_, res) = program.runWithRiggedCoin(true)
        res shouldBe 20
      }
    }
    "the coin flip result is tails" should {
      "not double the number" in {
        val (_, res) = program.runWithRiggedCoin(false)
        res shouldBe 10
      }
    }
  }
