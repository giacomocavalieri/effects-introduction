module Free.Stepper.Interpreters (stepperToIOInterpreter) where

import Control.Monad (void)
import Free.Core (type (~>))
import Free.Stepper.Core (Mode (..), StepperDSL (..))

stepperToIOInterpreter :: StepperDSL ~> IO
stepperToIOInterpreter = \case
  AskValue msg parser k -> k <$> askValueIO msg parser
  NotifyStep mode msg k -> k <$> notifyStepIO mode msg

askValueIO :: String -> (String -> Maybe a) -> IO a
askValueIO msg parser = do
  putStrLn msg
  response <- getLine
  case parser response of
    Nothing -> putStrLn "Invalid input" >> askValueIO msg parser
    Just a -> pure a

notifyStepIO :: Mode -> String -> IO ()
notifyStepIO mode msg = do
  putStrLn msg
  case mode of
    Wait -> putStrLn "(press return key to continue)" >> void getLine
    NonInteractive -> pure ()
