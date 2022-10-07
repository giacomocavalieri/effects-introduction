module Free.Core.Interpreters.Stepper (interpretAppToStepper) where

import Control.Monad.Free (iterM)
import Free.Core (App, AppDSL (..), CoinFlip, CoinFlipDSL (FlipCoin), Console, ConsoleDSL (PrintLine))
import Free.Stepper.Core (Stepper, askBoundedEnum, notifyStepAndWait)

-- One can get creative with the possible interpreters, this one for example translates
-- the App DSL to the Stepper one so that it could be interpreted with the ad hoc stepper
-- interpreters

interpretAppToStepper :: App a -> Stepper a
interpretAppToStepper = iterM $ \case
  EvalCoinFlip c k -> interpretCoinToStepper c >>= k
  EvalConsole c k -> interpretConsoleToStepper c >>= k

interpretCoinToStepper :: CoinFlip a -> Stepper a
interpretCoinToStepper = iterM $ \case
  FlipCoin k -> askBoundedEnum "What is the flip result? " >>= k

interpretConsoleToStepper :: Console a -> Stepper a
interpretConsoleToStepper = iterM $ \case
  PrintLine msg k -> notifyStepAndWait msg >>= k