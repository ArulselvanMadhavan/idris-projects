module Main

import Data.Vect

data DataStore : Type where
  -- Because size is not part of the type constructor, it is possible to create
  -- a vect of size that doesn't match with (size DataStore)
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

size : DataStore -> Nat
size (MkData size items) = size

items : (store: DataStore) -> Vect (size store) String
items (MkData size items) = items

newitems : (items : Vect size String) -> (y : String) -> Vect (S size) String
newitems [] y = [y]
newitems (x :: xs) y = x :: newitems xs y

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData (S size) (newitems items y)

parseCommand : (cmd : String) -> (arg : String) -> Maybe Command
parseCommand "add" arg = Just $ Add arg
parseCommand "get" arg = case all isDigit (unpack arg) of
                              False => Nothing
                              True  => Just $ Get (cast arg)
parseCommand "search" arg = Just $ Search arg
parseCommand "size" _  = Just Size
parseCommand "quit" _  = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, arg) => parseCommand cmd arg

getEntry : (x : Integer) -> (store : DataStore) -> (String, DataStore)
getEntry x store = case integerToFin x (size store) of
                        Nothing => ("Out of Range\n", store)
                        (Just i) => ((index i $ items store) ++ "\n", store)

-- I could make the result type of this function to be a string but I
-- worked hard to get the current return type working. I am going to
-- leave it as an example for future reference.
getSearchResults : (items : Vect n String) -> (searchTerm : String) -> (p ** Vect p (Nat, String))
getSearchResults {n = Z} [] searchTerm = (_ ** [])
getSearchResults {n = (S len)} (x :: xs) searchTerm =
  let (_ ** tail) = getSearchResults xs searchTerm
  in
  if Strings.isInfixOf searchTerm x then
  (_ ** ((len, x)) :: tail)
  else (_ ** tail)

-- printResults : (p ** Vect p (Nat, String)) -> String
-- printResults (x ** pf) = ?printResults_rhs_1

-- printResults [] = ""
-- printResults ((a, b) :: xs) = show a ++ "\t" ++ show b ++ "\n" ++ printResults xs

printResults : (p : Nat ** Vect p (Nat, String)) -> String
printResults (_ ** []) = ""
printResults (_ ** ((a, b) :: xs)) = show a ++ "\t" ++ show b ++ "\n" ++ (printResults (_ ** xs))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                            Nothing => Just ("Invalid command\n", store)
                            (Just (Add x)) => Just $ ("ID " ++ show (size store) ++ "\n", addToStore store x)
                            (Just (Get x)) => Just $ (getEntry x store)
                            (Just (Search x)) => Just $ (printResults $ getSearchResults (items store) x, store)
                            (Just Size) => Just $ ("Size is:" ++ show (size store) ++ "\n", store)
                            (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
