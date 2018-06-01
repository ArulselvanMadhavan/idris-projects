module Ex2

import BSTree
import Ex1

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ (val :: treeToList right)
