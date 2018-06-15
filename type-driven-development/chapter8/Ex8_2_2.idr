module Ex8_2_2

import Data.Vect

helper : Vect ((S n) + len) a -> Vect (plus n (S len)) a
helper {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs

reverse' : Vect n a -> Vect m a -> Vect (n + m) a
reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
reverse' {n} acc (x :: xs) = helper (reverse' (x :: acc) xs)

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
