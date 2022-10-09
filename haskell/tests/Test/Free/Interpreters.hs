{-# LANGUAGE FlexibleContexts #-}

module Test.Free.Interpreters (runWithRiggedCoin) where

import Control.Monad.RWS.Class (MonadWriter (tell))
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Trans.Reader (runReader)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Free.Core (App, AppDSL (..), CoinFlipDSL (..), ConsoleDSL (..), runWith)

runWithRiggedCoin :: App a -> Bool -> (a, String)
runWithRiggedCoin app = runReader $ runWriterT $ app `runWith` testInterpreter
 where
  coinFlipInterpreter (FlipCoin k) = k <$> ask
  consoleTestInterpreter (PrintLine msg k) = k <$> tell msg
  testInterpreter = \case
    EvalCoinFlip c k -> k <$> c `runWith` coinFlipInterpreter
    EvalConsole c k -> k <$> c `runWith` consoleTestInterpreter
