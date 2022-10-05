module Main (main) where

import MTL.Core (maybeDouble)
import MTL.Core.Interpreters.Production (runProductionApp)
import MTL.Core.Interpreters.Stepper (StepperApp (..))
import MTL.Stepper.Interpreters (runIOStepper)

main :: IO ()
main = do
  let stepByStepProgram = runIOStepper $ runStepperApp $ maybeDouble 10
  let productionProgram = runProductionApp $ maybeDouble 10
  putStrLn "Run interactively? [y/n]"
  res <-
    getLine >>= \case
      "y" -> stepByStepProgram
      _ -> productionProgram
  putStrLn $ "The result is: " <> show res
