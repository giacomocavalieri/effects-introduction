package mtl.stepper

import cats.syntax.all.*
import cats.Monad
import scala.util.Try
import cats.kernel.BoundedEnumerable

object Core:
  enum Mode:
    case Wait, NonInteractive

  // The Stepper ability, the possible actions are:
  //   - askValue to stop the execution and ask for a generic value
  //   - notifyStep to notify the execution of a step with a given message

  trait Stepper[F[_]]:
    def askValue[A](msg: String)(parser: String => Option[A]): F[A]
    def notifyStep(mode: Mode)(msg: String): F[Unit]

  object Stepper:
    import Mode.*

    // Utility method to get the correct instance in the for comprehension code
    def apply[F[_]](using s: Stepper[F]): Stepper[F] = s

    def notifyStepAndWait[F[_]: Stepper](msg: String): F[Unit] = Stepper[F].notifyStep(Wait)(msg)

    // Ask the user to choose a value between some choices
    def askChoice[F[_]: Stepper: Monad, A](options: Seq[A])(msg: String): F[A] =
      val numberedOptions: Seq[(A, Int)] = options.zipWithIndex
      val optionToString: ((A, Int)) => String = { case (a, n) => f"$n. $a" }
      for
        _ <- Stepper[F].notifyStep(NonInteractive)(msg)
        _ <- numberedOptions.traverse(o => Stepper[F].notifyStep(NonInteractive)(optionToString(o)))
        n <- Stepper[F].askValue("Number of the choosen value: ")(s => Try(s.toInt).toOption)
        choice <- numberedOptions.find(_._2 === n) match
          case None =>
            Stepper[F].notifyStep(NonInteractive)("Invalid number")
              *> askChoice(options)(msg)
          case Some((choice, _)) => choice.pure
      yield choice

    // Ask the user to choose a value between all the possible values of a bounded enumeration
    def askBoundedEnum[F[_]: Stepper: Monad, A: BoundedEnumerable](msg: String): F[A] =
      askChoice(BoundedEnumerable[A].membersAscending)(msg)
