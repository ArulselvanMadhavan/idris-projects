module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S k) = do putStrLn (show (S k))
                     usleep 1000000
                     countdown k

mutual
  invalidInput : IO ()
  invalidInput = do
    putStrLn "Invalid input"
    countdowns

  countdowns : IO ()
  countdowns = do
    putStr "Enter starting number:"
    Just startNum <- readNumber | Nothing => invalidInput
    countdown startNum
    putStr "Another (y/n)? "
    yn <- getLine
    if yn == "y" then countdowns
                 else pure ()
