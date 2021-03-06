module Ex8_3_2

import public Data.Fin

data MyVect : (n : Nat) -> (a : Type) -> Type where
  Nil  : MyVect Z a
  (::) : (x : a) -> (xs : MyVect n a) -> MyVect (S n) a

headUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} -> {contra : (x = y) -> Void} -> ((x :: xs) = (y :: ys)) -> Void
headUnequal {contra = contra} Refl = contra Refl

tailUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} -> {contra : (xs = ys) -> Void} -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal {contra} Refl = contra Refl

DecEq a => DecEq (MyVect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   (Yes Refl) => case decEq xs ys of
                                                      (Yes Refl) => Yes Refl
                                                      (No contra) => No (tailUnequal {contra = contra})
                                   (No contra) => No (headUnequal {contra = contra})
