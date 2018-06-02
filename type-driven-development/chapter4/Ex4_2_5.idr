module Ex4_2_5

import Data.Vect

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just x) => Just $ (Vect.index x xs) + (Vect.index x ys)
