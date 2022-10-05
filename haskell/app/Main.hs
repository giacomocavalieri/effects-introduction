module Main (main) where

import MTL.Core (maybeDouble)
import MTL.Interpreters.Stepper (StepperApp (..))
import Stepper.Interpreters (runIOStepper)

main :: IO ()
main = do
  n <- runIOStepper $ runStepperApp $ maybeDouble 10
  putStrLn $ "The result is: " <> show n
