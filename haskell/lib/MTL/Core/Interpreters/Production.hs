module MTL.Core.Interpreters.Production (ProductionApp (..)) where

import MTL.Core (CoinFlip (flipCoin), Console (printLine))
import System.Random (randomIO)

-- In production one could use the IO monad
-- The implementation of the action (i.e. how they are interpreted) may vary based on the
-- monad used, here we are using the IO monad so it is pretty straightforward to turn
-- the flipCoin and printLine actions to IO side effects

newtype ProductionApp a = ProductionApp {runProductionApp :: IO a}
  deriving (Functor, Applicative, Monad)

instance CoinFlip ProductionApp where
  flipCoin :: ProductionApp Bool
  flipCoin = ProductionApp randomIO

instance Console ProductionApp where
  printLine :: String -> ProductionApp ()
  printLine = ProductionApp . putStrLn