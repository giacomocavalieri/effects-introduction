module Free.Core.Interpreters.Production (appToIOInterpreter) where

import Free.Core (AppDSL (..), CoinFlipDSL (..), ConsoleDSL (..), runWith, type (~>))
import System.Random (randomIO)

-- To interpret a program one has to define how to interpret
-- each step of the possible actions.
-- Since the app is a simple product of Coin and Console its
-- implementation simply uses the single interpreters

appToIOInterpreter :: AppDSL ~> IO
appToIOInterpreter = \case
  EvalCoinFlip c k -> c `runWith` coinToIOInterpreter >>= k
  EvalConsole c k -> c `runWith` consoleToIOInterpreter >>= k

coinToIOInterpreter :: CoinFlipDSL ~> IO
coinToIOInterpreter = \case
  FlipCoin k -> randomIO >>= k

consoleToIOInterpreter :: ConsoleDSL ~> IO
consoleToIOInterpreter = \case
  PrintLine msg k -> putStrLn msg >> k ()
