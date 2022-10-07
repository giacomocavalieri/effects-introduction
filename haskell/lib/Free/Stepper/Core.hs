{-# LANGUAGE DeriveFunctor #-}

module Free.Stepper.Core (
  Mode (..),
  Stepper,
  StepperDSL (..),
  askValue,
  notifyStep,
  notifyStepAndWait,
  askChoice,
  askBoundedEnum,
) where

import Control.Monad.Free (Free, liftF)
import Data.List (find)
import Text.Read (readMaybe)

data Mode = Wait | NonInteractive

-- The Stepper ability, the possible actions are:
--   - askValue to stop the execution and ask for a generic value
--   - notifyStep to notify the execution of a step with a given message

type Stepper = Free StepperDSL
data StepperDSL next
  = forall a. AskValue String (String -> Maybe a) (a -> next)
  | NotifyStep Mode String (() -> next)
deriving instance Functor StepperDSL

askValue :: String -> (String -> Maybe a) -> Stepper a
askValue msg parser = liftF $ AskValue msg parser id

notifyStep :: Mode -> String -> Stepper ()
notifyStep mode msg = liftF $ NotifyStep mode msg id

notifyStepAndWait :: String -> Stepper ()
notifyStepAndWait = notifyStep Wait

-- Ask the user to choose a value between some choices
-- Note how the implementation is exactly the same as the one for MTL,
-- the only thing that changes is the type signature
askChoice :: Show a => [a] -> String -> Stepper a
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
askBoundedEnum :: (Bounded a, Enum a, Show a) => String -> Stepper a
askBoundedEnum = askChoice [minBound .. maxBound]
