module ExactLength

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

exactLength : (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength {m} len x = case m == len of
                             False => Nothing
                             True  => ?res
