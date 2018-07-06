module ArithCmd

import Arith
import System
import Ex11_2_1

||| Supported Input commands
public export
data Command : Type -> Type where
     PutStr  : String -> Command ()
     GetLine : Command String
     Pure    : ty -> Command ty
     Bind    : Command a -> (a -> Command b) -> Command b

||| ConsoleIO
public export
data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b


namespace CommandDo
  export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run (More _) (Quit y) = pure (Just y)
run (More fuel) (Do cmd f) = do x <- runCommand cmd
                                run fuel (f x)

mutual
  export
  correct : Stream Int -> (Score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct\n"
                          quiz nums (score + 1)
  export
  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                            quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: (num2 :: nums)) score =
    do PutStr ("Score so far: " ++ show score ++ "\n")
       PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
       answer <- GetLine
       if toLower answer == "quit" then Quit score else
          if (cast answer == num1 * num2)
             then correct nums score
             else wrong nums (num1 * num2) score

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0) | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
