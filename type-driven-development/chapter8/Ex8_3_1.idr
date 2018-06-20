 module Ex8_3_1

 import Data.Vect

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> {contra : (x = y) -> Void} -> ((x :: xs) = (y :: ys)) -> Void
headUnequal {contra = contra} Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> {contra : (xs = ys) -> Void} -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal {contra} Refl = contra Refl
