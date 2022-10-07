module Main (main) where

import qualified Free.App as Free (maybeDouble)
import Free.Core (runWith)
import qualified Free.Core.Interpreters.Production as Free
import qualified Free.Core.Interpreters.Stepper as Free
import qualified Free.Stepper.Interpreters as Free
import qualified MTL.Core as MTL (maybeDouble)
import qualified MTL.Core.Interpreters.Production as MTL (runProductionApp)
import qualified MTL.Core.Interpreters.Stepper as MTL (StepperApp (..))
import qualified MTL.Stepper.Interpreters as MTL (runIOStepper)

main :: IO ()
main = do
  let msg = "1. run the MTL example\n2. run the Free example\nChoose one of"
  res <- choose msg "1" "2" mainMTL mainFree
  putStrLn $ "The result is: " <> show res

choose :: String -> String -> String -> IO a -> IO a -> IO a
choose msg opta optb a b = do
  putStrLn $ mconcat [msg, " [", opta, "/", optb, "]"]
  getLine >>= \case
    s | s == opta -> a
    s | s == optb -> b
    s -> putStrLn (s <> " is not one of the two options") >> choose msg opta optb a b

chooseYN :: String -> IO a -> IO a -> IO a
chooseYN msg = choose msg "y" "n"

mainMTL :: IO Int
mainMTL = chooseYN "Run interactively?" stepByStepProgram productionProgram
 where
  stepByStepProgram = MTL.runIOStepper $ MTL.runStepperApp $ MTL.maybeDouble 10
  productionProgram = MTL.runProductionApp $ MTL.maybeDouble 10

mainFree :: IO Int
mainFree = chooseYN "Run interactively?" stepByStepProgram productionProgram
 where
  stepByStepProgram =
    Free.maybeDouble 10
      `runWith` Free.appToStepperInterpreter
      `runWith` Free.stepperToIOInterpreter
  productionProgram = Free.maybeDouble 10 `runWith` Free.appToIOInterpreter
