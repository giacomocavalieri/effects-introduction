package mtl.core.interpreters

import mtl.core.*
import cats.effect.IO

// In production one could use the IO monad
// The implementation of the action (i.e. how they are interpreted) may vary based on the
// monad used, here we are using the IO monad so it is pretty straightforward to turn
// the flipCoin and printLine actions to IO side effects

object Production:
  given CoinFlip[IO] with
    override def flipCoin: IO[Boolean] = IO(util.Random.nextBoolean)

  given Console[IO] with
    override def printLine(s: String): IO[Unit] = IO.println(s)
