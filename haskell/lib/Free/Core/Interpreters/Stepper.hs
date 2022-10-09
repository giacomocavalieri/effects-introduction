module Free.Core.Interpreters.Stepper (appToStepperInterpreter) where

import Free.Core (AppDSL (..), CoinFlipDSL (FlipCoin), ConsoleDSL (PrintLine), runWith, type (~>))
import Free.Stepper.Core (Stepper, askBoundedEnum, notifyStepAndWait)

-- One can get creative with the possible interpreters, this one for example translates
-- the App DSL to the Stepper one so that it could be interpreted with the ad hoc stepper
-- interpreters

appToStepperInterpreter :: AppDSL ~> Stepper
appToStepperInterpreter = \case
  EvalCoinFlip c k -> k <$> c `runWith` coinToStepperInterpreter
  EvalConsole c k -> k <$> c `runWith` consoleToStepperInterpreter

coinToStepperInterpreter :: CoinFlipDSL ~> Stepper
coinToStepperInterpreter = \case
  FlipCoin k -> k <$> askBoundedEnum "What is the flip result? "

consoleToStepperInterpreter :: ConsoleDSL ~> Stepper
consoleToStepperInterpreter = \case
  PrintLine msg k -> k <$> notifyStepAndWait msg