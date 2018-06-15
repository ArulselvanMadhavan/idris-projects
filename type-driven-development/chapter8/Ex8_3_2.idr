module Ex8_3_2

import public Data.Fin

data MyVect : (n : Nat) -> (a : Type) -> Type where
  Nil  : MyVect Z a
  (::) : (x : a) -> (xs : MyVect n a) -> MyVect (S n) a

DecEq a => DecEq (MyVect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   (Yes Refl) => ?result_3
                                   (No contra) => ?result_2
