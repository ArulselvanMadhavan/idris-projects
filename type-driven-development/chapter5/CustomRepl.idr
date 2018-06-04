module Main

repl' : String -> (String -> String) -> IO ()
repl' x f = do
  putStr x
  i <- getLine
  putStr $ f x

replWith' : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
replWith' state displayString f = do
  putStrLn displayString
  input <- getLine
  case f state input of
    Nothing  => pure ()
    (Just (outputString, nextState)) => do
                                        putStrLn outputString
                                        replWith' nextState displayString f
