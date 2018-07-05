module TreeLabel

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty))
                "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node x y z) = flatten x ++ (y :: flatten z)
