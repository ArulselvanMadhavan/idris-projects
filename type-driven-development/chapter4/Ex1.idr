module Ex1

import BSTree

%access export

total listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = BSTree.insert x (listToTree xs)
