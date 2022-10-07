module Free.App (maybeDouble) where

import Free.Core (App, evalCoin, evalConsole)
import qualified Free.Core as C

flipCoin :: App Bool
flipCoin = evalCoin C.flipCoin

printLine :: String -> App ()
printLine = evalConsole . C.printLine

-- Simple program combining theese effects:
-- it simply _describes_ the core domain logic. How this description will
-- be evaluated depends on the concrete `m`. This makes it easy to plug a different
-- `m` if the code is being run in production or being tested!
maybeDouble :: Int -> App Int
maybeDouble n = do
  printLine "Flipping a coin!"
  heads <- flipCoin
  pure $ if heads then n * 2 else n
