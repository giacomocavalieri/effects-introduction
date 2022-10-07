module Test.Free (test) where

import Free.App (maybeDouble)
import Test.Free.Interpreters (runWithRiggedCoin)
import Test.Hspec (SpecWith, context, describe, it, shouldBe)

-- With the test interpreter testing the core logic is a breeze!
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
