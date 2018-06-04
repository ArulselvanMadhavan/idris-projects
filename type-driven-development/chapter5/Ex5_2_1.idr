module Main

import System
import Effects
import Effect.Random

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

mutual
  message : String -> IO ()
  message msg = do
    putStrLn msg

  checkGuess : Nat -> Nat -> IO ()
  checkGuess target x =
    case compare x target of
         LT => do
               message "Guess higher!"
               guess target
         EQ => do
               message "Bingo! You got it!"
         GT => do
               message "Guess lower!"
               guess target


  guess : (target : Nat) -> IO ()
  guess target = do
    putStr "Enter your guess: "
    num <- readNumber
    case num of
         Nothing  => do
                     message "Invalid Input. Try again."
                     guess target
         (Just x) => checkGuess target x


main : IO ()
main = do
  t <- time
  guess (fromInteger (t `mod` 100))




-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
