module MTL (CoinFlip (..), Console (..), maybeDouble) where

import System.Random (randomIO)

class CoinFlip m where
  flipCoin :: m Bool

class Console m where
  printLine :: String -> m ()

maybeDouble :: (Monad m, CoinFlip m, Console m) => Int -> m Int
maybeDouble n = do
  printLine "Flipping a coin!"
  heads <- flipCoin
  pure $ if heads then n * 2 else n

instance CoinFlip IO where
  flipCoin :: IO Bool
  flipCoin = randomIO

instance Console IO where
  printLine :: String -> IO ()
  printLine = putStrLn
