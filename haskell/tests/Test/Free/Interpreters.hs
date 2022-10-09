module Test.Free.Interpreters (runWithRiggedCoin) where

import Control.Monad.Free (iterM)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Free.Core (App, AppDSL (..), CoinFlip, CoinFlipDSL (..), Console, ConsoleDSL (..), runWith, type (~>))

runWithRiggedCoin :: App a -> Bool -> (a, String)
runWithRiggedCoin app coinResult = runWriter $ app `runWith` interpreter
 where
  interpreter :: AppDSL ~> Writer String
  interpreter = \case
    EvalCoinFlip c k -> k <$> interpretTestCoinFlip coinResult c
    EvalConsole c k -> k <$> interpretTestConsole c

interpretTestCoinFlip :: Monad m => Bool -> CoinFlip a -> m a
interpretTestCoinFlip coinResult = iterM $ \case
  FlipCoin k -> k coinResult

interpretTestConsole :: Console a -> Writer String a
interpretTestConsole = iterM $ \case
  PrintLine msg k -> tell msg >> k ()
