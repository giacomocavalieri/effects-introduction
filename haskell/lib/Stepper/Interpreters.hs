module Stepper.Interpreters (IOStepper (..)) where

import Control.Monad (void)
import Stepper.Core (Mode (..), Stepper (askValue, notifyStep))

newtype IOStepper a = IOStepper {runIOStepper :: IO a}
  deriving (Functor, Applicative, Monad)

instance Stepper IOStepper where
  askValue :: String -> (String -> Maybe a) -> IOStepper a
  askValue msg parser = IOStepper go
   where
    go = do
      putStrLn msg
      response <- getLine
      case parser response of
        Nothing -> putStrLn "Invalid input" >> go
        Just a -> pure a

  notifyStep :: Mode -> String -> IOStepper ()
  notifyStep mode msg = IOStepper $ do
    putStrLn msg
    case mode of
      Wait -> putStrLn "(press return key to continue)" >> void getLine
      NonInteractive -> pure ()
