module Ex10_2_3

import Data.Nat.Views

toBinaryHelper : (k : Nat) -> List Char
toBinaryHelper k with (halfRec k)
  toBinaryHelper Z | HalfRecZ = []
  toBinaryHelper (n + n) | (HalfRecEven evenRec) = (::) '0' (toBinaryHelper n | evenRec)
  toBinaryHelper (S (n + n)) | (HalfRecOdd oddRec) = (::) '1' (toBinaryHelper n | oddRec)

||| Returns the binary representation of a Nat
toBinary' : (k : Nat) -> List Char
toBinary' k = reverse (toBinaryHelper k)
