package mtl

import cats.data.{ReaderT, Writer}
import Example.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

// We can define a custom F to test the core logic of our function. Here we can use
// a specific monad stack: a Reader will allow us to mock the coin flip result
// a Writer will allow us to log the console output
type TestApp[A] = ReaderT[Writer[String, _], Boolean, A]

extension [A](a: TestApp[A])
  def runWithRiggedCoin(coinResult: Boolean): (String, A) =
    a.run(coinResult).run

given CoinFlip[TestApp] with
  def flipCoin: TestApp[Boolean] = ReaderT.ask

given Console[TestApp] with
  def printLine(s: String): TestApp[Unit] = ReaderT(_ => Writer.tell(s))

// Now testing the core logic is a breeze!
class Test extends AnyWordSpec, Matchers:
  val program = maybeDouble[TestApp](10)
  "maybeDouble" when {
    "executed" should {
      "always print a message regardless of the coin flip result" in {
        program.runWithRiggedCoin(true)._1 shouldBe "Flipping a coin!"
        program.runWithRiggedCoin(false)._1 shouldBe "Flipping a coin!"
      }
    }
    "the coin flip result is heads" should {
      "double the number" in {
        program.runWithRiggedCoin(true)._2 shouldBe 20
      }
    }
    "the coin flip result is tails" should {
      "not double the number" in {
        program.runWithRiggedCoin(false)._2 shouldBe 10
      }
    }
  }
