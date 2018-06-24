module Ex10_2_3

import Data.Nat.Views

toBinary' : (k : Nat) -> List Char
toBinary' k with (halfRec k)
  toBinary' Z | HalfRecZ = ['4']
  toBinary' (n + n) | (HalfRecEven evenRec) = (::) '0' (toBinary' n | evenRec)
  toBinary' (S (n + n)) | (HalfRecOdd oddRec) = (::) '1' (toBinary' n | oddRec)

-- main : IO (List Char)
-- main = do pure (toBinary' 42)
