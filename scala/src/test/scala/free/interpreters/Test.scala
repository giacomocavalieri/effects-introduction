package free.interpreters

import free.core.*
import free.core.AppDSL.*
import free.core.CoinFlipDSL.*
import free.core.ConsoleDSL.*
import cats.{~>, Monad}
import cats.data.Writer
import cats.syntax.all.*

object Test:
  extension [A](a: App[A])
    def runWithRiggedCoin(coinValue: Boolean): (String, A) =
      a.runWith(testAppInterpreter(coinValue)).run

  def testCoinFlipInterpreter[M[_]: Monad](coinValue: Boolean): CoinFlipDSL ~> M =
    new (CoinFlipDSL ~> M):
      override def apply[A](c: CoinFlipDSL[A]): M[A] = c match
        case FlipCoin => coinValue.pure

  def testConsoleInterpreter: ConsoleDSL ~> Writer[String, _] =
    new (ConsoleDSL ~> Writer[String, _]):
      override def apply[A](c: ConsoleDSL[A]): Writer[String, A] = c match
        case PrintLine(msg) => Writer.tell(msg)

  def testAppInterpreter(coinValue: Boolean): AppDSL ~> Writer[String, _] =
    new (AppDSL ~> Writer[String, _]):
      override def apply[A](a: AppDSL[A]): Writer[String, A] = a match
        case EvalCoinFlip(c) => c.foldMap(testCoinFlipInterpreter(coinValue))
        case EvalConsole(c)  => c.foldMap(testConsoleInterpreter)
