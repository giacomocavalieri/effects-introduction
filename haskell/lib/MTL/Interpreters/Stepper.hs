module MTL.Interpreters.Stepper (StepperApp (..)) where

import MTL.Core (CoinFlip (flipCoin), Console (printLine))
import Stepper.Core (Stepper, askBoundedEnum, notifyStepAndWait)

-- One can get creative with the possible interpreters, this one for example translates the
-- CoinFlip and Console ability to any monad that can provide the Stepper ability:
-- it allows to execute the program step-by-step asking the user to input the values it needs
-- to keep the computation going

newtype StepperApp m a = StepperApp {runStepperApp :: m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Stepper m) => CoinFlip (StepperApp m) where
  flipCoin :: StepperApp m Bool
  flipCoin = StepperApp $ askBoundedEnum "What is the flip result? "

instance Stepper m => Console (StepperApp m) where
  printLine :: String -> StepperApp m ()
  printLine = StepperApp . notifyStepAndWait
