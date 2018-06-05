module Ex6_2_3

import Data.Vect

TupleVect : (n : Nat) -> (t: Type) -> Type
TupleVect Z t = ()
TupleVect (S k) t = (t, TupleVect k t)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
