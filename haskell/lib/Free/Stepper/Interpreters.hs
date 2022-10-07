module Free.Stepper.Interpreters (interpretStepper) where

import Control.Monad (void)
import Control.Monad.Free (iterM)
import Free.Stepper.Core (Mode (..), Stepper, StepperDSL (..))

interpretStepper :: Stepper a -> IO a
interpretStepper = iterM $ \case
  AskValue msg parser k -> askValueIO msg parser >>= k
  NotifyStep mode msg k -> notifyStepIO mode msg >>= k

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
