module DataStore

import Data.Vect

data DataStore : Type where
  -- Because size is not part of the type constructor, it is possible to create
  -- a vect of size that doesn't match with (size DataStore)
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Get Integer
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
parseCommand "quit" _  = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, arg) => parseCommand cmd arg

getEntry : (x : Integer) -> (store : DataStore) -> (String, DataStore)
getEntry x store = case integerToFin x (size store) of
                        Nothing => ("Out of Range\n", store)
                        (Just i) => ((index i $ items store) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                            Nothing => Just ("Invalid command\n", store)
                            (Just (Add x)) => Just $ ("ID " ++ show (size store) ++ "\n", addToStore store x)
                            (Just (Get x)) => Just $ (getEntry x store)
                            (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command:" processInput
