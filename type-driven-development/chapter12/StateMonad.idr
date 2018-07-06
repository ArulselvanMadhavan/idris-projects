module StateMonad

import TreeLabelType
-- import Control.Monad.State

mutual
  Functor (State stateType) where
    map f x = Bind x (\val => Pure (f val))

  Applicative (State stateType) where
    pure = Pure
    (<*>) f a = Bind f (\f' =>
                Bind a (\a' =>
                Pure (f' a')))

  Monad (State stateType) where
    (>>=) = Bind

addIfPositive : Integer -> State Integer Bool
addIfPositive x = do when (x > 0) $
                          do current <- Get
                             Put (current + x)
                     pure (x > 0)

addPositives : List Integer -> State Integer Nat
addPositives xs = do added <- traverse addIfPositive xs
                     pure (length (filter id added))
