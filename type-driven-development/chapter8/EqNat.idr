module EqNat

import Data.Vect

data EqNat : (num1 : Nat) -> (nunm2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just eq) => Just (sameS _ _ eq)

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat' Z Z = Just (Same 0)
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               Nothing => Nothing
                               (Just (Same j)) => Just (Same (S j))

checkEqNat'' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat'' Z Z = Just (Same 0)
checkEqNat'' Z (S k) = Nothing
checkEqNat'' (S k) Z = Nothing
checkEqNat'' (S k) (S j) = do (Same eq) <- checkEqNat'' k j
                              pure (Same (S eq))


exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat'' m len of
                                 Nothing  => Nothing
                                 (Just (Same len)) => Just input
