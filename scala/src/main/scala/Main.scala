import mtl.*
import mtl.Example.*
import mtl.given
import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple:
  val run = for
    res <- maybeDouble[IO](10)
    _ <- IO.println(f"The result is: $res")
  yield ()
