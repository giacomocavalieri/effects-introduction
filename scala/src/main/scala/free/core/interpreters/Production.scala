package free.core.interpreters

import cats.effect.IO
import free.core.{App, ConsoleDSL, AppDSL, CoinFlipDSL}
import free.core.App.*
import free.core.AppDSL.*
import free.core.CoinFlipDSL.*
import free.core.ConsoleDSL.*
import cats.~>
import free.core.CoinFlipDSL

object Production:
  val productionInterpreter = new (AppDSL ~> IO):
    override def apply[A](a: AppDSL[A]): IO[A] = a match
      case EvalCoinFlip(c) => c.foldMap(productionCoinFlipInterpreter)
      case EvalConsole(c)  => c.foldMap(productionConsoleInterpreter)

  val productionCoinFlipInterpreter = new (CoinFlipDSL ~> IO):
    override def apply[A](cf: CoinFlipDSL[A]): IO[A] = cf match
      case FlipCoin => IO(util.Random.nextBoolean)

  val productionConsoleInterpreter = new (ConsoleDSL ~> IO):
    override def apply[A](c: ConsoleDSL[A]): IO[A] = c match
      case PrintLine(msg) => IO.println(msg)
