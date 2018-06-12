module Ex8_1_1

same_cons_rhs_1 : {xs : List a} -> {ys : List a} -> (prf : xs = ys) -> (x :: xs) = (x :: ys)
same_cons_rhs_1 prf = cong prf

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = same_cons_rhs_1 prf
