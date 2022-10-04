module MTL (CoinFlip (..), Console (..), maybeDouble) where

import System.Random (randomIO)

-- The effects we will need to perform, we can be as fine-grained or coarse-grained as we want
-- with the type classes
-- For this example it makes sense to have two distinct type classes:
-- one for the effect of flipping a coin and one for the console-related effects
class CoinFlip m where
  flipCoin :: m Bool

class Console m where
  printLine :: String -> m ()

-- Simple program combining theese effects:
maybeDouble :: (Monad m, CoinFlip m, Console m) => Int -> m Int
maybeDouble n = do
  printLine "Flipping a coin!"
  heads <- flipCoin
  pure $ if heads then n * 2 else n

-- maybeDouble simply _describes_ the core domain logic. How this description will
-- be evaluated depends on the concrete `m`. This makes it easy to plug a different
-- `m` if the code is being run in production or being tested!

-- In production one could use the IO monad
-- The implementation of the action (i.e. how they are interpreted) may vary based on the
-- monad used, here we are using the IO monad so it is pretty straightforward
instance CoinFlip IO where
  flipCoin :: IO Bool
  flipCoin = randomIO

instance Console IO where
  printLine :: String -> IO ()
  printLine = putStrLn
