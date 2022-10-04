module Main (main) where

import Test.Hspec (describe, hspec)
import Test.MTL (test)

main :: IO ()
main = hspec $ do
  describe "mtl tests" Test.MTL.test