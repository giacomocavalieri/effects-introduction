module MTL.Stepper.Core (Mode (..), Stepper (..), askChoice, askBoundedEnum, notifyStepAndWait) where

import Data.List (find)
import Text.Read (readMaybe)

data Mode = Wait | NonInteractive

-- The Stepper ability, the possible actions are:
--   - askValue to stop the execution and ask for a generic value
--   - notifyStep to notify the execution of a step with a given message

class Stepper m where
  askValue :: String -> (String -> Maybe a) -> m a
  notifyStep :: Mode -> String -> m ()

notifyStepAndWait :: Stepper m => String -> m ()
notifyStepAndWait = notifyStep Wait

-- Ask the user to choose a value between some choices
askChoice :: (Monad m, Stepper m, Show a) => [a] -> String -> m a
askChoice options msg = do
  let numberedOptions = zip [1 :: Int ..] options
  let optionToString (n, o) = show n <> ". " <> show o
  notifyStep NonInteractive msg
  mapM_ (notifyStep NonInteractive . optionToString) numberedOptions
  n <- askValue "Number of the choosen value: " readMaybe
  case find ((== n) . fst) numberedOptions of
    Just (_, choice) -> pure choice
    Nothing -> notifyStep NonInteractive "Invalid number" >> askChoice options msg

-- Ask the user to choose a value between all the possible values of a bounded enumeration
askBoundedEnum :: (Monad m, Stepper m, Bounded a, Enum a, Show a) => String -> m a
askBoundedEnum = askChoice [minBound .. maxBound]
