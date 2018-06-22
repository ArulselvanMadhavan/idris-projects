module Ex10_2_1

import Data.List.Views

equalSuffixHelper : Eq a => List a -> List a -> List a
equalSuffixHelper xs ys with (snocList ys)
  equalSuffixHelper xs [] | Empty = []
  equalSuffixHelper xs (zs ++ [x]) | (Snoc ysrec) with (snocList xs)
    equalSuffixHelper [] (zs ++ [x]) | (Snoc ysrec) | Empty = []
    equalSuffixHelper (ys ++ [y]) (zs ++ [x]) | (Snoc ysrec) | (Snoc xsrec) = case x == y of
      False => []
      True => x :: (equalSuffixHelper ys zs | ysrec | xsrec)

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys = reverse $ equalSuffixHelper xs ys
