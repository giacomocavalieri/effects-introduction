{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Free.Core (
  flipCoin,
  printLine,
  evalCoin,
  evalConsole,
  CoinFlipDSL (..),
  ConsoleDSL (..),
  AppDSL (..),
  CoinFlip,
  Console,
  App,
) where

import Control.Monad.Free (Free (..), liftF)

-- Our core domain actions, we can be as granular as we want
-- but if our program needs to use more than one DSL we need to combine
-- them in an algebra.
-- Here we combine the possible actions of FlipCoin and PrintLine
-- in a single App

data CoinFlipDSL next = FlipCoin (Bool -> next)
data ConsoleDSL next = PrintLine String (() -> next)
data AppDSL next
  = forall a. EvalCoinFlip (CoinFlip a) (a -> next)
  | forall a. EvalConsole (Console a) (a -> next)

deriving instance Functor CoinFlipDSL
deriving instance Functor ConsoleDSL
deriving instance Functor AppDSL

-- Now that our DSLs are Functors they can be wrapped in Free
-- obtaining a monad instance for _free_! (pun intended)
type CoinFlip = Free CoinFlipDSL
type Console = Free ConsoleDSL
type App = Free AppDSL

-- To make writing the code easier one can provide smart constructors
flipCoin :: CoinFlip Bool
flipCoin = liftF $ FlipCoin id

printLine :: String -> Console ()
printLine msg = liftF $ PrintLine msg id

evalCoin :: CoinFlip a -> App a
evalCoin c = liftF $ EvalCoinFlip c id

evalConsole :: Console a -> App a
evalConsole c = liftF $ EvalConsole c id
