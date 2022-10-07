package free.core

import cats.{Monad, ~>}
import cats.free.Free
import cats.free.Free.liftF

// Definition of the flip coin DSL
enum CoinFlipDSL[A]:
  case FlipCoin extends CoinFlipDSL[Boolean]
// Wrap the DSL in Free to get a monad instance
type CoinFlip[A] = Free[CoinFlipDSL, A]
// Smart constructors for the DSL operation
object CoinFlip:
  def flipCoin: CoinFlip[Boolean] = liftF(CoinFlipDSL.FlipCoin)

// Definition of the flip coin DSL
enum ConsoleDSL[A]:
  case PrintLine(msg: String) extends ConsoleDSL[Unit]
// Wrap the DSL in Free to get a monad instance
type Console[A] = Free[ConsoleDSL, A]
// Smart constructor for the DSL operation
object Console:
  def printLine(msg: String): Console[Unit] = liftF(ConsoleDSL.PrintLine(msg))

// An application that uses both effects
enum AppDSL[A]:
  case EvalCoinFlip[B](c: CoinFlip[B]) extends AppDSL[B]
  case EvalConsole[B](c: Console[B]) extends AppDSL[B]
// Wrap the app DSL in Free to get a monad instance
type App[A] = Free[AppDSL, A]
// Smart constructors for the DSL operations
object App:
  def evalCoinFlip[A](c: CoinFlip[A]): App[A] = liftF(AppDSL.EvalCoinFlip(c))
  def evalConsole[A](c: Console[A]): App[A] = liftF(AppDSL.EvalConsole(c))
  def flipCoin: App[Boolean] = evalCoinFlip(CoinFlip.flipCoin)
  def printLine(msg: String): App[Unit] = evalConsole(Console.printLine(msg))

extension [DSL[_], A](a: Free[DSL, A])
  def runWith[F[_]: Monad](interpreter: DSL ~> F): F[A] = a.foldMap(interpreter)

def maybeDouble(n: Int): App[Int] =
  for
    _ <- App.printLine("Flipping a coin!")
    heads <- App.flipCoin
  yield (if heads then n * 2 else n)
