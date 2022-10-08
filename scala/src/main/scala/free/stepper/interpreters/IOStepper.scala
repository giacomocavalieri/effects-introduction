package free.stepper.interpreters

import cats.~>
import cats.effect.IO
import free.stepper.Core.*
import free.stepper.Core.StepperDSL.*

object IOStepper:
  val stepperToIOInterpreter = new (StepperDSL ~> IO):
    override def apply[A](s: StepperDSL[A]): IO[A] = s match
      case AskValue(msg, parser) => askValue(msg, parser)
      case NotifyStep(mode, msg) => notifyStep(mode, msg)

  private def askValue[A](msg: String, parser: String => Option[A]): IO[A] =
    for
      _ <- IO.println(msg)
      response <- IO.readLine
      res <- parser(response) match
        case None    => IO.println("Invalid input") *> askValue(msg, parser)
        case Some(a) => IO.pure(a)
    yield res

  private def notifyStep(mode: Mode, msg: String): IO[Unit] =
    for
      _ <- IO.println(msg)
      _ <- mode match
        case Mode.Wait           => IO.println("(press return key to continue)") *> IO.readLine
        case Mode.NonInteractive => IO.pure(())
    yield ()
