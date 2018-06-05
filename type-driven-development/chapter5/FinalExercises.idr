module FinalExercises

import Data.Vect

readHelper : List String -> IO (List String)
readHelper xs = do
  line <- getLine
  if line == "" then pure xs else readHelper (line :: xs)

readToBlank : IO (List String)
readToBlank = readHelper []

join : List String -> (c : Char) -> String
join [] _ = ""
join (x :: []) c = x ++ join [] c
join (x :: (y :: xs)) c = x ++ singleton c ++ join (y :: xs) c

readAndSave : IO ()
readAndSave = do
  allLines <- readToBlank
  putStr "Enter a filename(with extension): "
  fileName <- getLine
  _ <- writeFile fileName (join (reverse allLines) '\n') | Left err => putStrLn (show err)
  pure ()

readIntoVector : (fh : File) -> IO (n : Nat ** Vect n String)
readIntoVector fh = do
  False <- fEOF fh | True => pure (_ ** [])
  Right line <- fGetLine fh | Left err => do putStrLn (show err)
                                             pure (_ ** [])
  (_ ** lines) <- readIntoVector fh
  pure (_ ** (trim line) :: lines)

readVectFile : (fileName : String) -> IO (n ** Vect n String)
readVectFile fileName = do
  Right fh <- openFile fileName Read | Left err => do putStrLn (show err)
                                                      pure (_ ** [])
  readIntoVector fh
