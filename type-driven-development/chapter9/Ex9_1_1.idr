module Ex9_1_1

data Elem' : a -> List a -> Type where
     Here  : Elem' x (x :: xs)
     There : (later : Elem' x xs) -> Elem' x (y :: xs)
