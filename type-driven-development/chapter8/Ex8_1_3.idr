module Ex8_1_3

import Data.Vect

||| Three values must be equal
||| Holding a value of this type is a proof
||| that all three values are equal
data ThreeEq : a -> b -> c -> Type where
  Same : (t : a) -> ThreeEq t t t

||| The parameter ThreeEq requires that first three arguments
||| be equal
allSameS : (x, y, z : Nat) -> (xyz : ThreeEq x y z) -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Same z) = Same (S z)
