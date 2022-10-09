module Test.Free.Interpreters (runWithRiggedCoin) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Free.Core (App, AppDSL (..), CoinFlipDSL (..), ConsoleDSL (..), runWith, type (~>))

type TestApp = WriterT String (Reader Bool)

runWithRiggedCoin :: App a -> Bool -> (a, String)
runWithRiggedCoin app = runReader $ runWriterT $ app `runWith` interpreter
 where
  interpreter = \case
    EvalCoinFlip c k -> k <$> c `runWith` coinFlipInterpreter
    EvalConsole c k -> k <$> c `runWith` consoleTestInterpreter

coinFlipInterpreter :: CoinFlipDSL ~> TestApp
coinFlipInterpreter (FlipCoin k) = k <$> lift ask

consoleTestInterpreter :: ConsoleDSL ~> TestApp
consoleTestInterpreter (PrintLine msg k) = k <$> tell msg
