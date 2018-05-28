module Main

palindrome : Nat -> String -> Bool
palindrome l s =
  let ls = (toLower s)
      len = length s
  in
  if (len < l) then False else ls == (reverse ls)

main : IO String
main = do
  putStrLn "Enter a string: "
  s <- getLine
  putStrLn "Enter minimum palindrome requirement length: "
  i <- getLine
  pure $ show (palindrome (cast i) s)
