module Ex10_2_3

import Data.Nat.Views

toBinaryHelper : (k : Nat) -> List Char
toBinaryHelper k with (halfRec k)
  toBinaryHelper Z | HalfRecZ = []
  toBinaryHelper (n + n) | (HalfRecEven rec) = '0' :: (toBinaryHelper n | rec)
  toBinaryHelper (S (n + n)) | (HalfRecOdd rec) = '1' :: (toBinaryHelper n | rec)


toBinary : Nat -> List Char
toBinary k = toBinaryHelper k
