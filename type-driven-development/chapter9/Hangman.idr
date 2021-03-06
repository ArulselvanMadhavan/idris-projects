module Hangman

import Data.Vect
import RemoveElem

data WordState : (guess_rem : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) -> (missing : Vect letters Char) -> WordState guess_rem letters

%name WordState st

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won  : (game : WordState (S guesses) 0) -> Finished

||| Predicate to be used for validating inputs.
data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]


emptyInput : ValidInput [] -> Void
emptyInput (Letter _) impossible

moreChars : ValidInput (x :: (y :: xs)) -> Void
moreChars (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No emptyInput
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No moreChars

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               s <- getLine
               case isValidString (toUpper s) of
                    -- Case splitting on prf results in impossible cases
                    (Yes prf) => pure (_ ** prf)
                    (No contra) => do putStrLn "Invalid guess"
                                      readGuess

processGuess : (letter : Char) -> WordState (S guesses) (S letters) -> Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      (Yes prf)   => Right (MkWordState word (removeElem letter missing prf))
                                                      (No contra) => Left (MkWordState word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter c) <- readGuess
                                 case processGuess c st of
                                      (Left l) => do putStrLn "Wrong!"
                                                     case guesses of
                                                          Z => pure (Lost l)
                                                          (S k) => game l
                                      (Right r) => do putStrLn "Correct!"
                                                      case letters of
                                                           Z => pure (Won r)
                                                           (S k) => game r

main : IO ()
main = do result <- game {guesses = 2} (MkWordState "Test" ['S', 'E', 'T'])
          case result of
               (Lost (MkWordState word missing)) => putStrLn ("You lose. The word was " ++ word)
               (Won game) => putStrLn "You win!"
