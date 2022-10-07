package free.stepper

import cats.free.Free
import cats.free.Free.liftF
import cats.kernel.BoundedEnumerable
import cats.syntax.all.*
import scala.util.Try

object Core:
  enum Mode:
    case Wait, NonInteractive

  enum StepperDSL[A]:
    case AskValue[B](msg: String, parser: String => Option[B]) extends StepperDSL[B]
    case NotifyStep(mode: Mode, msg: String) extends StepperDSL[Unit]

  type Stepper[A] = Free[StepperDSL, A]

  object Stepper:
    import StepperDSL.*
    import Mode.*

    def notifyStep(mode: Mode)(msg: String): Stepper[Unit] = liftF(NotifyStep(mode, msg))
    def askValue[A](msg: String)(parser: String => Option[A]): Stepper[A] =
      liftF(AskValue(msg, parser))

    def notifyStepAndWait(msg: String): Stepper[Unit] = notifyStep(Wait)(msg)

    // Ask the user to choose a value between some choices
    def askChoice[A](options: Seq[A])(msg: String): Stepper[A] =
      val numberedOptions: Seq[(A, Int)] = options.zipWithIndex
      val optionToString: ((A, Int)) => String = { case (a, n) => f"$n. $a" }
      for
        _ <- notifyStep(NonInteractive)(msg)
        _ <- numberedOptions.traverse(o => notifyStep(NonInteractive)(optionToString(o)))
        n <- askValue("Number of the choosen value: ")(s => Try(s.toInt).toOption)
        choice <- numberedOptions.find(_._2 === n) match
          case None => notifyStep(NonInteractive)("Invalid number") *> askChoice(options)(msg)
          case Some((choice, _)) => Free.pure(choice)
      yield choice

    // Ask the user to choose a value between all the possible values of a bounded enumeration
    def askBoundedEnum[A: BoundedEnumerable](msg: String): Stepper[A] =
      askChoice(BoundedEnumerable[A].membersAscending)(msg)
