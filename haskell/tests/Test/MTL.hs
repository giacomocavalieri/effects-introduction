module Test.MTL (test) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import MTL (CoinFlip (..), Console (..), maybeDouble)
import Test.Hspec (SpecWith, context, describe, it, shouldBe)

newtype TestApp a = TestApp (ReaderT Bool (Writer String) a) deriving (Functor, Applicative, Monad)

runWithRiggedCoin :: TestApp a -> Bool -> (a, String)
runWithRiggedCoin (TestApp app) coinResult = runWriter $ runReaderT app coinResult

instance CoinFlip TestApp where
  flipCoin :: TestApp Bool
  flipCoin = TestApp ask

instance Console TestApp where
  printLine :: String -> TestApp ()
  printLine = TestApp . lift . tell

test :: SpecWith ()
test = do
  let program = maybeDouble 10
  describe "maybeDouble" $ do
    context "when executed" $ do
      it "should output a message" $ do
        let (_, msg1) = program `runWithRiggedCoin` True
        let (_, msg2) = program `runWithRiggedCoin` False
        msg1 `shouldBe` "Flipping a coin!"
        msg2 `shouldBe` "Flipping a coin!"

    context "when the coin flip result is heads" $ do
      it "should double the number" $ do
        let (res, _) = program `runWithRiggedCoin` True
        res `shouldBe` 20

    context "when the coin flip result is tails" $ do
      it "should not double the number" $ do
        let (res, _) = program `runWithRiggedCoin` False
        res `shouldBe` 10
