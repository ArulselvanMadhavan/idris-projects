module Main

counts : String -> (Nat, Nat)
counts s =
  let w = length $ split isSpace s
      c = length s
  in (w, c)

main : IO ()
main = repl "Enter a String: " (show . counts)
