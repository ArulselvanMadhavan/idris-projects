module SnocList

data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc  : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelper {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelper {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                                snocListHelper (Snoc {x} snoc) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper Empty xs

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse xs = myReverseHelper xs (snocList xs)
