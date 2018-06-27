module Ex11_1_2

|||InfList is similar to stream
data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
  map f (value :: xs) = (f value) :: (map f xs)

countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z x = []
getPrefix (S k) (value :: xs) = value :: (getPrefix k xs)
