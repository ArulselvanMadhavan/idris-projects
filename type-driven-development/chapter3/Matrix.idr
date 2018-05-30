module Matrix

import Data.Vect

transposeMat_rhs_3 : Vect n (Vect 0 a)
transposeMat_rhs_3 = ?fill

transposeMat_rhs_2 : (x : Vect n a) -> (xs : Vect len (Vect n a)) -> Vect n (Vect (S len) a)
transposeMat_rhs_2 x xs = ?transposeMat_rhs_2_rhs

transposeMat : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = transposeMat_rhs_3
transposeMat (x :: xs) = transposeMat_rhs_2 x xs
