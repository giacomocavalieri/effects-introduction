module Free.Core.Interpreters.Production (appToIOInterpreter) where

import Free.Core (AppDSL (..), CoinFlipDSL (..), ConsoleDSL (..), runWith, type (~>))
import System.Random (randomIO)

-- To interpret a program one has to define how to interpret
-- each step of the possible actions.
-- Since the app is a simple product of Coin and Console its
-- implementation simply uses the single interpreters

appToIOInterpreter :: AppDSL ~> IO
appToIOInterpreter = \case
  EvalCoinFlip c k -> k <$> c `runWith` coinToIOInterpreter
  EvalConsole c k -> k <$> c `runWith` consoleToIOInterpreter

coinToIOInterpreter :: CoinFlipDSL ~> IO
coinToIOInterpreter = \case
  FlipCoin k -> k <$> randomIO

consoleToIOInterpreter :: ConsoleDSL ~> IO
consoleToIOInterpreter = \case
  PrintLine msg k -> k <$> putStrLn msg
