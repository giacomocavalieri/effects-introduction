package mtl.core

import cats.Monad
import cats.syntax.all.*
import cats.effect.IO

// The effects we will need to perform, we can be as fine-grained or coarse-grained as we want
// with the type classes
// For this example it makes sense to have two distinct type classes:
// one for the effect of flipping a coin and one for the console-related effects
trait CoinFlip[F[_]]:
  def flipCoin: F[Boolean]

trait Console[F[_]]:
  def printLine(s: String): F[Unit]

// Utility objects to get the correct instances writing code in the for comprehension
object CoinFlip:
  def apply[F[_]](using c: CoinFlip[F]): CoinFlip[F] = c

object Console:
  def apply[F[_]](using c: Console[F]): Console[F] = c

// Simple program combining theese effects:
// it simply _describes_ the core domain logic. How this description will
// be evaluated depends on the concrete `m`. This makes it easy to plug a different
// `m` if the code is being run in production or being tested!
def maybeDouble[F[_]: Monad: CoinFlip: Console](n: Int): F[Int] =
  for
    _ <- Console[F].printLine("Flipping a coin!")
    heads <- CoinFlip[F].flipCoin
  yield (if heads then n * 2 else n)
