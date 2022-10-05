package mtl.core.interpreters

import mtl.stepper.Core.Stepper
import mtl.core.{Console, CoinFlip}
import cats.Monad

// One can get creative with the possible interpreters, this one for example translates the
// CoinFlip and Console ability to any monad that can provide the Stepper ability:
// it allows to execute the program step-by-step asking the user to input the values it needs
// to keep the computation going

object StepperInterpreter:
  given coinFlipForStepper[M[_]: Stepper: Monad]: CoinFlip[M] with
    override def flipCoin: M[Boolean] = Stepper.askBoundedEnum("What is the flip result?")

  given consoleForStepper[M[_]: Stepper]: Console[M] with
    override def printLine(msg: String): M[Unit] = Stepper.notifyStepAndWait(msg)
