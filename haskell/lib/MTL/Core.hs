module MTL.Core (CoinFlip (..), Console (..), maybeDouble) where

-- The effects we will need to perform, we can be as fine-grained or coarse-grained as we want
-- with the type classes
-- For this example it makes sense to have two distinct type classes:
-- one for the effect of flipping a coin and one for the console-related effects
class CoinFlip m where
  flipCoin :: m Bool

class Console m where
  printLine :: String -> m ()

-- Simple program combining theese effects:
-- it simply _describes_ the core domain logic. How this description will
-- be evaluated depends on the concrete `m`. This makes it easy to plug a different
-- `m` if the code is being run in production or being tested!
maybeDouble :: (Monad m, CoinFlip m, Console m) => Int -> m Int
maybeDouble n = do
  printLine "Flipping a coin!"
  heads <- flipCoin
  pure $ if heads then n * 2 else n
