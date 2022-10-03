module Main (main) where

import MTL (maybeDouble)

main :: IO ()
main = do
  n <- maybeDouble 10
  putStrLn $ "The result is: " <> show n
