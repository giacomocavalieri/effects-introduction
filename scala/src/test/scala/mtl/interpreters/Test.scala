package mtl.interpreters

import mtl.core.{Console, CoinFlip}
import cats.data.{ReaderT, Writer}

// For testing one can define a custom type to test the core logic of our function.
// Here we can use a specific monad stack: a Reader will allow us to mock the coin flip
// result and a Writer will allow us to log the console output

object Test:
  // Note that `Writer[String, _]` is a lambda on types
  // (i.e. Writer[String, _] === [A] =>> Writer[String, A])
  // enabled by the compiler plugin -Ykind-projector:underscores, it allows to define type-level
  // lambdas with the same syntax as term-level lambdas
  type TestApp[A] = ReaderT[Writer[String, _], Boolean, A]

  extension [A](a: TestApp[A])
    def runWithRiggedCoin(coinResult: Boolean): (String, A) =
      a.run(coinResult).run

  given CoinFlip[TestApp] with
    override def flipCoin: TestApp[Boolean] = ReaderT.ask

  given Console[TestApp] with
    override def printLine(s: String): TestApp[Unit] = ReaderT(_ => Writer.tell(s))
