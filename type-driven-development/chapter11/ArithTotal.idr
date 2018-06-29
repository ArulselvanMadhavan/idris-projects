module ArithTotal

import Data.Primitives.Views
import System
import Arith

%default total

public export
data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

public export
data Fuel = Dry | More Fuel

export
run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More x) (Do y f) = do c <- y
                           run x (f c)

export partial
forever : Fuel
forever = More forever

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (num1 :: (num2 :: nums)) score =
  do putStrLn ("Score so far: " ++ show score)
     putStr (show num1 ++ " * " ++ show num2 ++ "? ")
     answer <- getLine
     if (cast answer == num1 * num2)
        then do putStrLn "Correct" --Uses the >>= for InfIO
                quiz nums (score + 1)
        else do putStrLn "Wrong"
                quiz nums score

partial
main : IO ()
main = do seed <- time
          run forever (quiz (arithInputs (fromInteger seed)) 0)
