module Main

import System

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

  checkGuess : Nat -> Nat -> Nat -> IO ()
  checkGuess guesses target x =
    case compare x target of
         LT => do
               message "Guess higher!"
               guess target guesses
         EQ => do
               message "Bingo! You got it!"
         GT => do
               message "Guess lower!"
               guess target guesses


  guess : (target : Nat) -> (guesses : Nat) ->IO ()
  guess target guesses = do
    putStrLn $ "Total guesses so far: " ++ (show guesses)
    putStr "Enter your guess: "
    num <- readNumber
    case num of
         Nothing  => do
                     message "Invalid Input. Try again."
                     guess target (S guesses)
         (Just x) => checkGuess (S guesses) target x


main : IO ()
main = do
  t <- time
  guess (fromInteger (t `mod` 100)) 0




-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
