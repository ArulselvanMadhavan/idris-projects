module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data DataStore : Type where
  -- Because size is not part of the type constructor, it is possible to create
  -- a vect of size that doesn't match with (size DataStore)
  MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore

data Command : Schema -> Type where
  Add  : SchemaType schema -> Command schema
  Get  : Integer -> Command schema
  Size : Command schema
  Search : SchemaType schema -> Command schema
  Quit : Command schema

schema : DataStore -> Schema
schema (MkData schema' _ _) = schema'

size : DataStore -> Nat
size (MkData schema' size' items') = size'

items : (store: DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema' size' items') = items'

newItems : (schema : Schema) -> (xs : Vect k (SchemaType schema)) -> (y : SchemaType schema) -> Vect (S k) (SchemaType schema)
newItems schema [] y = [y]
newItems schema (x :: xs) y = x :: (newItems _ xs y)

addToStore : (store: DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema Z items) y = MkData schema _ (y :: items)
addToStore (MkData schema (S k) (x :: xs)) y = MkData schema _ (x :: (newItems schema xs y))

extractText : (xs : List Char) -> Maybe (String, List Char)
extractText [] = Nothing -- No closing quote found
extractText ('"' :: xs) = Just (ltrim . pack $ xs, [])
extractText (x :: xs) = (map . map) ((::) x) (extractText xs)

parseString : (arg : List Char) -> Maybe (String, String)
parseString ('"' :: xs) = (map . map) pack (extractText xs)
parseString _ = Nothing

parseInt : (arg : String) -> Maybe (String, Int)
parseInt arg = case span isDigit arg of
                    ("", b) => Nothing
                    (a, b)  => Just (ltrim b, (cast a))

parsePrefix : (schema : Schema) -> (arg : String) -> Maybe (String, SchemaType schema)
parsePrefix SString arg = parseString (unpack arg)
parsePrefix SInt arg = parseInt arg
parsePrefix (x .+. y) arg = do (rem1, schema1) <- parsePrefix x arg
                               (rem2, schema2) <- parsePrefix y rem1
                               pure (rem2, (schema1, schema2))

parseBySchema : (schema : Schema) -> (arg : String) -> Maybe (SchemaType schema)
parseBySchema schema arg = do (rem, result) <- parsePrefix schema arg
                              if rem == "" then pure result else Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (arg : String) -> Maybe (Command schema)
parseCommand schema "add" arg = Add <$> parseBySchema schema arg
parseCommand schema "get" arg = case all isDigit (unpack arg) of
                                     False => Nothing
                                     True  => Just $ Get (cast arg)
parseCommand schema "search" arg = Search <$> (parseBySchema schema arg)
parseCommand schema "size" _  = Just Size
parseCommand schema "quit" _  = Just Quit
parseCommand schema _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, arg) => parseCommand schema cmd arg


display : SchemaType schema -> String
display {schema = SString} x = show x
display {schema = SInt} x = show x
display {schema = (y .+. z)} (m, n) = display m ++ ", " ++ display n

getEntry : (x : Integer) -> (store : DataStore) -> (String, DataStore)
getEntry x store = case integerToFin x (size store) of
                        Nothing => ("Out of Range\n", store)
                        (Just i) => (display (index i $ items store) ++ "\n", store)

-- -- I could make the result type of this function to be a string but I
-- -- worked hard to get the current return type working. I am going to
-- -- leave it as an example for future reference.
-- getSearchResults : (items : Vect n String) -> (searchTerm : String) -> (p ** Vect p (Nat, String))
-- getSearchResults {n = Z} [] searchTerm = (_ ** [])
-- getSearchResults {n = (S len)} (x :: xs) searchTerm =
--   let (_ ** tail) = getSearchResults xs searchTerm
--   in
--   if Strings.isInfixOf searchTerm x then
--   (_ ** ((len, x)) :: tail)
--   else (_ ** tail)

-- printResults : (p : Nat ** Vect p (Nat, String)) -> String
-- printResults (_ ** []) = ""
-- printResults (_ ** ((a, b) :: xs)) = show a ++ "\t" ++ show b ++ "\n" ++ (printResults (_ ** xs))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case  parse (schema store) input of
                             Nothing => Just ("Invalid command\n", store)
                             Just (Add x) => Just $ ("ID " ++ show (size store) ++ "\n", addToStore store x)
                             Just (Get x) => Just $ (getEntry x store)
                             -- Just (Search x) => Just $ (printResults $ getSearchResults (items store) x, store)
                             Just Size => Just $ ("Size is:" ++ show (size store) ++ "\n", store)
                             Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
