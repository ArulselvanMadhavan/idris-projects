module Ex6_2_1

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)
