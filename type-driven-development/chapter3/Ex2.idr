module Ex2

import Data.Vect

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: (addMatrix xs ys)
