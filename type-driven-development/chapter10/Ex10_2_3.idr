module Ex10_2_3

import Data.Nat.Views

toBinary' : (k : Nat) -> String
toBinary' k with (halfRec k)
  toBinary' Z | HalfRecZ = ""
  toBinary' (n + n) | (HalfRecEven rec) = "0" ++ (toBinary' n | rec)
  toBinary' (S (n + n)) | (HalfRecOdd rec) = "1" ++ (toBinary' n | rec)
