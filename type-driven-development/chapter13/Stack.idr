module Stack

import Data.Vect

data StackCmd : (elem : Type) -> Nat -> Nat -> Type where
  Push : (item : elem) -> StackCmd elem height (S height)
  Pop  : StackCmd elem (S height) (height)
  Top  : StackCmd elem (S height) (S height)
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd elem height1 height2 -> (elem -> StackCmd elem height2 height3) -> StackCmd elem height1 height3

testAdd : StackCmd Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)
