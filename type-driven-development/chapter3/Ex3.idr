module Ex3

import Data.Vect

createEmpties : Vect m (Vect 0 a)
createEmpties = replicate _ []

transpose' : Vect n (Vect m a) -> Vect m (Vect n a)
transpose' [] = createEmpties
transpose' (x :: xs) = let xstrans = transpose' xs in
                     zipWith (::) x xstrans

multCore : Num a => (x : Vect n a) -> (y : Vect n a) -> a
multCore x y = sum $ zipWith (*) x y

multRowCol : Num a => (x : Vect n a) -> (xsTrans : Vect k (Vect n a)) -> Vect k a
multRowCol x [] = []
multRowCol x (y :: xs) = multCore x y :: multRowCol y xs

multHelper : Num a => (xs : Vect m (Vect n a)) -> (xsTrans : Vect k (Vect n a)) -> Vect m (Vect k a)
multHelper [] [] = []
multHelper (x :: xs) xsTrans = multRowCol x xsTrans :: multHelper xs xsTrans

multMatrix : (Num a) => Vect m (Vect n a) -> Vect n (Vect k a) -> Vect m (Vect k a)
multMatrix xs ys = let ysTrans = transpose' ys in
                   multHelper xs ysTrans
