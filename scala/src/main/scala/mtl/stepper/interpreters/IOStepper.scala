package mtl.stepper.interpreters

import mtl.stepper.Core.Stepper
import mtl.stepper.Core.Mode
import cats.effect.IO
import cats.syntax.all.*

// Interpret the stepper in the IO monad

object IOStepper:
  given Stepper[IO] with
    override def askValue[A](msg: String)(parser: String => Option[A]): IO[A] =
      for
        _ <- IO.println(msg)
        response <- IO.readLine
        res <- parser(response) match
          case None    => IO.println("Invalid input") *> askValue(msg)(parser)
          case Some(a) => IO.pure(a)
      yield res

    override def notifyStep(mode: Mode)(msg: String): IO[Unit] =
      for
        _ <- IO.println(msg)
        _ <- mode match
          case Mode.Wait           => IO.println("(press return key to continue)") *> IO.readLine
          case Mode.NonInteractive => IO.pure(())
      yield ()
