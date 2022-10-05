module MTL.Interpreters.Stepper (StepperApp (..)) where

import MTL.Core (CoinFlip (flipCoin), Console (printLine))
import Stepper.Core (askBoundedEnum, notifyStepAndWait)
import Stepper.Interpreters (IOStepper)

-- One can get creative with the possible interpreters, this one for example translates the
-- CoinFlip and Console ability in the IOStepper monad: that is a custom monad that allows
-- to execute the program step-by-step asking the user to input the values it needs to keep
-- the computation going

newtype StepperApp a = StepperApp {runStepperApp :: IOStepper a}
  deriving (Functor, Applicative, Monad)

instance CoinFlip StepperApp where
  flipCoin :: StepperApp Bool
  flipCoin = StepperApp $ askBoundedEnum "What is the flip result? "

instance Console StepperApp where
  printLine :: String -> StepperApp ()
  printLine = StepperApp . notifyStepAndWait
