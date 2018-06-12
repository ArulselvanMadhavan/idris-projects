module Ex8_1_2

same_lists_rhs : {xs : List a} -> {ys : List a} -> (prf : x = y) -> (prf1 : xs = ys) -> (x :: xs) = (y :: ys)
-- same_lists_rhs prf prf1 = cong {f = prf} prf1

same_lists : { xs : List a } -> { ys : List a } -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists prf prf1 = same_lists_rhs prf prf1
