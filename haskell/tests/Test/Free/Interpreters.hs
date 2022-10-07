module Test.Free.Interpreters (runWithRiggedCoin) where

import Control.Monad.Free (iterM)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Free.Core (App, AppDSL (..), CoinFlip, CoinFlipDSL (..), Console, ConsoleDSL (..))

runWithRiggedCoin :: App a -> Bool -> (a, String)
runWithRiggedCoin app coinResult = runWriter $ go app
 where
  go = iterM $ \case
    EvalCoinFlip c k -> interpretTestCoinFlip coinResult c >>= k
    EvalConsole c k -> interpretTestConsole c >>= k

interpretTestCoinFlip :: Monad m => Bool -> CoinFlip a -> m a
interpretTestCoinFlip coinResult = iterM $ \case
  FlipCoin k -> k coinResult

interpretTestConsole :: Console a -> Writer String a
interpretTestConsole = iterM $ \case
  PrintLine msg k -> tell msg >> k ()
