module SuffixCheck
import SnocList

isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with (snocList xs)
  isSuffix [] ys | Empty = True
  isSuffix (zs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    isSuffix (zs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (zs ++ [x]) (xs ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = case x == y of
         False => False
         True => isSuffix zs xs | xsrec | ysrec
