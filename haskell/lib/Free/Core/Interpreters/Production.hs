module Free.Core.Interpreters.Production (interpretApp) where

import Control.Monad.Free (iterM)
import Free.Core (App, AppDSL (..), CoinFlip, CoinFlipDSL (..), Console, ConsoleDSL (..))
import System.Random (randomIO)

-- To interpret a program one has to define how to interpret
-- each step of the possible actions.
-- Since the app is a simple product of Coin and Console its
-- implementation simply uses the single interpreters

interpretApp :: App a -> IO a
interpretApp = iterM $ \case
  EvalCoinFlip c k -> interpretCoin c >>= k
  EvalConsole c k -> interpretConsole c >>= k

interpretCoin :: CoinFlip a -> IO a
interpretCoin = iterM $ \case
  FlipCoin k -> randomIO >>= k

interpretConsole :: Console a -> IO a
interpretConsole = iterM $ \case
  PrintLine msg k -> putStrLn msg >> k ()
