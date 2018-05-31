module Matrix

import Data.Vect

createEmpties : Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeHelper : (x : Vect n a) -> (xsTrans : Vect n (Vect len a)) -> Vect n (Vect (S len) a)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans
