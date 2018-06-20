module DescribeList

public export
data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

||| ListLast uses Dependent type
describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

export total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          (NonEmpty xs last) => NonEmpty (x :: xs) last

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

export
describeListEnd' : List Int -> String
describeListEnd' xs with (listLast xs)
  describeListEnd' [] | Empty = "Empty"
  describeListEnd' (ys ++ [x]) | (NonEmpty ys x) = "Non empty initial portion = " ++ show ys
