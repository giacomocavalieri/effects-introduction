module Test.MTL.Interpreters (TestApp, runWithRiggedCoin) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import MTL.Core (CoinFlip (flipCoin), Console (printLine))

-- For testing one can define a custom type to test the core logic of our function.
-- Here we can use a specific monad stack: a Reader will allow us to mock the coin flip
-- result and a Writer will allow us to log the console output

newtype TestApp a = TestApp (ReaderT Bool (Writer String) a)
  deriving (Functor, Applicative, Monad)

runWithRiggedCoin :: TestApp a -> Bool -> (a, String)
runWithRiggedCoin (TestApp app) coinResult = runWriter $ runReaderT app coinResult

instance CoinFlip TestApp where
  flipCoin :: TestApp Bool
  flipCoin = TestApp ask

instance Console TestApp where
  printLine :: String -> TestApp ()
  printLine = TestApp . lift . tell