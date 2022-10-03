{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MTLTest (tests) where

import MTL (CoinFlip (..), Console (..), maybeDouble)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Test.HUnit (Test (TestList), (~:), (~=?))

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

testMessageOutput :: Bool -> Test
testMessageOutput coinResult =
  let (_, msg) = maybeDouble 10 `runWithRiggedCoin` coinResult
   in "Flipping a coin!" ~=? msg

testCoinFlip :: Int -> Bool -> Test
testCoinFlip expected coinResult =
  let (res, _) = maybeDouble 10 `runWithRiggedCoin` coinResult
   in expected ~=? res

tests :: Test
tests =
  TestList
    [ "maybeDouble outputs a message when coin lands on heads" ~: testMessageOutput True
    , "maybeDouble outputs a message when coin lands on tails" ~: testMessageOutput False
    , "maybeDouble doubles the input when coin lands on heads" ~: testCoinFlip 20 True
    , "maybeDouble does not double the input when coin lands on tails" ~: testCoinFlip 10 False
    ]