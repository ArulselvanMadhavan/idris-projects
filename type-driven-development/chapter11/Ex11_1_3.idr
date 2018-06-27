module Ex11_1_3

import Data.Primitives.Views
import Arith

data Face : Type where
     Heads : Face
     Tails : Face

|||TODO: Need to figure out how to derive instances automatically.
Show Face where
  show Heads = "Heads"
  show Tails = "Tails"

isEven : (value : Int) -> Bool
isEven value with (divides value 2)
  isEven ((2 * div) + rem) | (DivBy prf) = if (rem == 0) then True else False

coinFlips : (n : Nat) -> Stream Int -> List Face
coinFlips Z _ = []
coinFlips (S k) (x :: xs) = case isEven x of
                                 False => Tails :: coinFlips k xs
                                 True => Heads :: coinFlips k xs

main : IO (List Face)
main = do pure $ coinFlips 10 (randoms 12345)
