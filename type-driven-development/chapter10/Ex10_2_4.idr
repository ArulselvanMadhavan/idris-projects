module Ex10_2_4

import Data.List.Views

palindrome : List Char -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = case x == y of
                                                     False => False
                                                     True => (palindrome ys | rec)
