module OverLength

over_length : Nat -> List String -> Nat
over_length i xs =
  length $ filter (((<) i) . length) xs
