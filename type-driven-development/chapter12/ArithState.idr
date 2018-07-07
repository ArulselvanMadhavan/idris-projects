module ArithState

import Arith
import System
import Ex11_2_1
import Data.Primitives.Views

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat

record GameState where
       constructor MkGameState
       score : Score
       difficulty : Int

Show GameState where
     show st = show (correct (score st)) ++ "/" ++
               show (attempted (score st)) ++ "\n" ++
               "Difficulty: " ++ show (difficulty st)

initState : GameState              
initState = MkGameState (MkScore 0 0) 12

||| Supported Input commands
data Command : Type -> Type where
     PutStr  : String -> Command ()
     GetLine : Command String
     
     GetRandom : Command Int
     GetGameState : Command GameState
     PutGameState : GameState -> Command ()
     
     Pure    : ty -> Command ty
     Bind    : Command a -> (a -> Command b) -> Command b

||| ConsoleIO
data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Input = Answer Int | QuitCmd

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))
                         
getRandom : Int -> Int -> Int
getRandom val max with (divides val max)
  getRandom val 0 | DivByZero = 1
  getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1

runCommand : Stream Int -> GameState -> Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do input <- getLine
                                   pure (input, rnds, state)
runCommand (val :: rnds) state GetRandom = pure ((getRandom val (difficulty state)), rnds, state)
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newstate) = pure ((), rnds, newstate)
runCommand rnds state (Pure x) = pure (x, rnds, state)
runCommand rnds state (Bind x f) = do (res, nextRnds, nextState) <- runCommand rnds state x
                                      runCommand nextRnds nextState (f res)

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run Dry rnds state _ = pure (Nothing, rnds, state)
run (More x) xs y (Quit z) = do pure (Just z, xs, y)
run (More x) xs y (Do w f) = do (res, newR, newS) <- runCommand xs y w
                                run x newR newS (f res)

addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

mutual
  correct : ConsoleIO GameState
  correct = do PutStr "Correct\n"
               st <- GetGameState
               PutGameState (addCorrect st)
               quiz
  
  wrong : Int -> ConsoleIO GameState
  wrong ans = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                 st <- GetGameState
                 PutGameState (addWrong st)
                 quiz
  
  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st   <- GetGameState
            PutStr (show st ++ "\n")
            input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
            case input of
                 Answer answer => if answer == num1 * num2
                                     then correct
                                     else wrong (num1 * num2)
                 QuitCmd => Quit st

-- partial
-- main : IO ()
-- main = do seed <- time
--           Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0) | Nothing => putStrLn "Ran out of fuel"
--           putStrLn ("Final score: " ++ show score)
