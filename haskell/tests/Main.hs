module Main (main) where

import MTLTest (tests)
import Test.HUnit (runTestTTAndExit)

main :: IO ()
main = runTestTTAndExit tests