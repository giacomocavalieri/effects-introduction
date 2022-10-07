package free.core.interpreters

import cats.~>
import free.core.*
import free.core.AppDSL.*
import free.core.CoinFlipDSL.*
import free.core.ConsoleDSL.*
import free.stepper.Core.*
import free.stepper.Core.Stepper.*
import cats.kernel.BoundedEnumerable.catsKernelBoundedEnumerableForBoolean

object Stepper:
  val appToStepperInterpreter: AppDSL ~> Stepper = new (AppDSL ~> Stepper):
    override def apply[A](a: AppDSL[A]): Stepper[A] = a match
      case EvalCoinFlip(c) => c.foldMap(stepperCoinFlipInterpreter)
      case EvalConsole(c)  => c.foldMap(stepperConsoleInterpreter)

  val stepperCoinFlipInterpreter: CoinFlipDSL ~> Stepper = new (CoinFlipDSL ~> Stepper):
    override def apply[A](c: CoinFlipDSL[A]): Stepper[A] = c match
      case FlipCoin => askBoundedEnum("What is the flip result?")

  val stepperConsoleInterpreter: ConsoleDSL ~> Stepper = new (ConsoleDSL ~> Stepper):
    override def apply[A](c: ConsoleDSL[A]): Stepper[A] = c match
      case PrintLine(msg) => notifyStepAndWait(msg)
