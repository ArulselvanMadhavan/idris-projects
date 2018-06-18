module Ex9_1_2

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInList : Last [] value -> Void
notInList LastOne impossible
notInList (LastCons _) impossible

notLast : (contra : (x = value) -> Void) -> Last [x] value -> Void
notLast contra LastOne = contra Refl
notLast _ (LastCons LastOne) impossible
notLast _ (LastCons (LastCons _)) impossible

notAvail : (contra : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notAvail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInList
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (notLast contra)
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                     (Yes prf)   => Yes (LastCons prf)
                                     (No contra) => No (notAvail contra)
