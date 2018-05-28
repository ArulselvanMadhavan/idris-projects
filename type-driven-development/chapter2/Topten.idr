module Topten

top_ten : Ord a => List a -> List a
top_ten xs =
  let xs_sorted = sort xs
  in
  take 10 xs_sorted
