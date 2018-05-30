module Exercise_3_2

import Data.Vect

total my_length : List a -> Nat
my_length [] = 0
my_length (_ :: xs) = 1 + (my_length xs)

total rev_helper : List a -> List a -> List a
rev_helper [] ys = ys
rev_helper (x :: xs) ys = rev_helper xs (x :: ys)

total my_reverse : List a -> List a
my_reverse xs = rev_helper xs []

-- my_reverse_helper : (ys : Vect k elem) -> (xs : Vect len elem) -> Vect (S k) elem
-- my_reverse_helper ys [] = ys
-- my_reverse_helper ys (x :: xs) = ?my_reverse_helper_rhs_2

-- my_reverse' : Vect len elem -> Vect len elem
-- my_reverse' xs = my_reverse_helper [] xs

total map : (a -> b) -> List a -> List b
map f [] = []
map f (x :: xs) = f x :: map f xs

total my_map : (a -> b) -> Vect len a -> Vect len b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs
