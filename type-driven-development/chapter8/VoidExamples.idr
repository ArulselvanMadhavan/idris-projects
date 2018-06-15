module VoidExamples

twoPlusTwoFive : 2 + 2 = 5 -> Void
twoPlusTwoFive Refl impossible

valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible
