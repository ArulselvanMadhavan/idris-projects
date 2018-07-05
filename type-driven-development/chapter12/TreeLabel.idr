module TreeLabel

public export
data Tree a = Empty
            | Node (Tree a) a (Tree a)

%default total

export
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty))
                "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))
export
flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = flatten x ++ (y :: flatten z)

treeLabelWith : (lbls : Stream labelType) -> Tree a -> (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node x y z)
  = let (lblThis :: lblLeft, left_t) = treeLabelWith lbls x
        (lblRight, right_t) = treeLabelWith lblLeft z
        in
        (lblRight, Node left_t (lblThis, y) right_t)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)
