module Stack

import Data.Vect

%default total

data StackCmd : (elem : Type) -> Nat -> Nat -> Type where
  Push : (item : elem) -> StackCmd elem height (S height)
  Pop  : StackCmd elem (S height) (height)
  Top  : StackCmd elem (S height) (S height)
  GetStr : StackCmd elem height height
  GetCmd : StackCmd String height height
  PutStr : String -> StackCmd () height height
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd elem height1 height2 -> (elem -> StackCmd elem height2 height3) -> StackCmd elem height1 height3

data StackIO : (a : Type) -> Nat -> Type where
  Do : (StackCmd a height1 height2) -> (a -> Inf (StackIO a height2)) -> StackIO a height1
  Bind : (StackCmd a height1 height2) -> (a -> Inf (StackIO b height2)) -> StackIO b height1

namespace StackDo
  (>>=) : (StackCmd a height1 height2) -> (a -> Inf (StackIO a height2)) -> StackIO a height1
  (>>=) = Bind

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

runStack : (Cast String a) => (stk : Vect inHeight a) -> StackCmd a inHeight outHeight -> IO (a, Vect outHeight a)
runStack {a} stk GetStr = do input <- getLine
                             pure (cast input, stk)
runStack stk GetCmd = do input <- getLine
                         pure (input, stk)
runStack stk (PutStr y) = do putStr y
                             pure ((), stk)
runStack stk (Push item) = pure (item, item :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack (x :: xs) Top = pure (x, x :: xs)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (newElem, newstk) <- runStack stk x
                            runStack newstk (f newElem)

partial
run : (Cast String a) => Fuel -> Vect height a -> StackIO a height -> IO ()
run Dry xs y = pure ()
run (More fuel) stk (Do c f) = do putStr "coming here"
                                  (res, newstk) <- runStack stk c
                                  run fuel newstk (f res)

data StkInput = Number Integer | Add

strToInput : String -> Maybe StkInput
strToInput "add" = Just Add
strToInput ""    = Nothing
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual
  tryAdd : StackIO Integer height
  tryAdd {height = (S (S h))} = Bind doAdd (\_ =>
                                Bind Top {a = Integer} (\res =>
                                Bind (PutStr (show res ++ "Show result \n")) (\_ => stackCalc)))
  tryAdd = Bind (PutStr "Fewer than two items on the stack\n") (\_ => stackCalc)

  stackCalc : StackIO Integer height
  stackCalc = Bind (PutStr "> ") (\_ =>
              Bind (GetCmd) (\v => case strToInput v of
                                        Nothing => Bind (PutStr "Enter a valid input") (\_ => stackCalc)
                                        (Just (Number x)) => do Push x
                                                                stackCalc
                                        (Just Add) => tryAdd))

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

partial
main : IO ()
main = run forever [] stackCalc
