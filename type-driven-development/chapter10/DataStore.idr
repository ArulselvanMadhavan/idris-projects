module Main

import Data.Vect
import Debug.Trace

infixr 5 .+.

public export
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

-- data DataStore : Type where
  -- Because size is not part of the type constructor, it is possible to create
  -- a vect of size that doesn't match with (size DataStore)
  -- MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore

export
record DataStore (schema : Schema) where
  constructor MkData
  size  : Nat
  items : Vect size (SchemaType schema)

data Command : Schema -> Type where
  Add  : SchemaType schema -> Command schema
  Get  : Maybe Integer -> Command schema
  SetSchema : (newSchema : Schema) -> Command schema
  Size : Command schema
  Quit : Command schema

-- schema : DataStore -> Schema
-- schema (MkData schema' _ _) = schema'

-- size : DataStore -> Nat
-- size (MkData schema' size' items') = size'

-- items : (store: DataStore schema) -> Vect (size store) (SchemaType schema)
-- items (MkData schema' size' items') = items'

newItems : (schema : Schema) -> (xs : Vect k (SchemaType schema)) -> (y : SchemaType schema) -> Vect (S k) (SchemaType schema)
newItems schema [] y = [y]
newItems schema (x :: xs) y = x :: (newItems _ xs y)

export
empty : DataStore schema
empty = MkData 0 []

export
addToStore : (SchemaType schema) -> (store: DataStore schema) ->  DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

public export
data StoreView : DataStore schema -> Type where
     SNil : StoreView empty
     SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (x :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelp items

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
                    (a, b)  => Just (b, (cast a))


parseChar : (arg : List Char) -> Maybe (String, Char)
parseChar (''' :: (c :: (''' :: xs))) = Just (pack xs, c)
parseChar _ = Nothing

parsePrefix : (schema : Schema) -> (arg : String) -> Maybe (String, SchemaType schema)
parsePrefix SString arg = parseString (unpack $ltrim arg)
parsePrefix SInt arg = parseInt (ltrim arg)
parsePrefix SChar arg = parseChar (unpack $ ltrim arg)
parsePrefix (x .+. y) arg = do (rem1, schema1) <- parsePrefix x arg
                               (rem2, schema2) <- parsePrefix y rem1
                               pure (rem2, (schema1, schema2))

parseBySchema : (schema : Schema) -> (arg : String) -> Maybe (SchemaType schema)
parseBySchema schema arg = do (rem, result) <- parsePrefix schema arg
                              if rem == "" then pure result else Nothing

parseSchema : (xs: List String) -> Maybe Schema
parseSchema ("String" :: []) = Just SString
parseSchema ("String" :: xs) = ((.+.) SString) <$> parseSchema xs
parseSchema ("Int" :: []) = Just SInt
parseSchema ("Int" :: xs) = ((.+.) SInt) <$> parseSchema xs
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (arg : String) -> Maybe (Command schema)
parseCommand schema "add" arg = Add <$> parseBySchema schema arg
parseCommand schema "get" "" = Just $ Get Nothing
parseCommand schema "get" arg = case all isDigit (unpack $ ltrim arg) of
                                     False => Just $ Get Nothing
                                     True  => Just $ Get (Just (cast arg))
parseCommand schema "schema" arg = SetSchema <$> (parseSchema (words arg))
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

joinVect : Char -> Vect p (SchemaType schema) -> String
joinVect _ [] = ""
joinVect c (x :: xs) = (display x) ++ (singleton c) ++ (joinVect c xs)

getEntries : (store : DataStore schema) -> (String, DataStore schema)
getEntries store = ((joinVect '\n' $ items store), store)

getEntry : (x : Integer) -> (store : DataStore schema) -> (String, DataStore schema)
getEntry x store = case integerToFin x (size store) of
                        Nothing => ("Out of Range\n", store)
                        (Just i) => (display (index i $ items store) ++ "\n", store)

setSchema : (schema : Schema) -> (store : DataStore schema) -> Maybe (String, DataStore schema)
setSchema schema store = if size store == 0
                         then Just ("Store updated with new schema\n", MkData _ [])
                         else Nothing

-- processInput : DataStore schema -> String -> Maybe (String, DataStore schema)
-- processInput store input = case parse schema input of
--                              Nothing => Just ("Invalid command\n", store)
--                              Just (Add x) => Just $ ("ID " ++ show (size store) ++ "\n", addToStore store x)
--                              Just (Get Nothing) => Just $ getEntries store
--                              Just (Get (Just x)) => Just $ (getEntry x store)
--                              Just Size => Just $ ("Size is:" ++ show (size store) ++ "\n", store)
--                              Just (SetSchema schema) => case (setSchema schema store) of
--                                Nothing => Just ("Unable to set schema.\n Store already has data.\n", store)
--                                (Just x) => Just x
--                              Just Quit => Nothing

-- main : IO ()
-- main = replWith (MkData _ []) "Command: " processInput
