import Control.Monad.State

export
increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)
