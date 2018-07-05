module Ex12_1_1

import Control.Monad.State
import TreeLabel

update : (stateType -> stateType) -> State stateType ()
update f = do current <- get
              let next = f current
              put next

increase : Nat -> State Nat ()
increase x = update (+x)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = put (S Z)
countEmpty (Node left val right) = do countEmpty left
                                      el <- get
                                      countEmpty right
                                      er <- get
                                      put (plus el er)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = put ((S Z), Z)
countEmptyNode (Node x y z) = do countEmptyNode x
                                 (el, nl) <- get
                                 countEmptyNode z
                                 (er, nr) <- get
                                 put (plus el er, S (plus nl nr))
