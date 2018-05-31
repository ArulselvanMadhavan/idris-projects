module Ex1

import Data.Vect

createEmpties : Vect m (Vect 0 a)
createEmpties = replicate _ []

tranpose' : Vect n (Vect m a) -> Vect m (Vect n a)
tranpose' [] = createEmpties
tranpose' (x :: xs) = let xstrans = transpose xs in
                     zipWith (::) x xstrans
