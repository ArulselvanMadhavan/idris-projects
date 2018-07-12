module Stack

import Data.Vect

data StackCmd : (elem : Type) -> Nat -> Nat -> Type where
  Push : (item : elem) -> StackCmd elem height (S height)
  Pop  : StackCmd elem (S height) (height)
  Top  : StackCmd elem (S height) (S height)
  GetStr : StackCmd elem height height
  PutStr : String -> StackCmd () height height
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd elem height1 height2 -> (elem -> StackCmd elem height2 height3) -> StackCmd elem height1 height3

toElem : (elem : Type) -> String -> Maybe elem

runStack : (Monoid elem) => (stk : Vect inHeight elem) -> StackCmd elem inHeight outHeight -> IO (elem, Vect outHeight elem)
runStack {elem} stk GetStr = do input <- getLine
                                case toElem elem input of
                                     Nothing => do putStrLn "Are you fucking stupid? Expected element is "
                                                   pure ?myRes
                                     (Just x) => ?res_2
runStack stk (PutStr y) = ?StackCmd_rhs_3
runStack stk (Push item) = pure (item, item :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack (x :: xs) Top = pure (x, x :: xs)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (newElem, newstk) <- runStack stk x
                            runStack newstk (f newElem)

testAdd : StackCmd Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)

doAdd : StackCmd Integer (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

-- runStack [2, 3, 4] (do doAdd; doAdd)
