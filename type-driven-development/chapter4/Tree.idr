module Tree

data Tree a = Empty
            | Node (Tree a) a (Tree a)

%name Tree tree, tree1

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node tree y tree1) = case compare x y of
                                    LT => insert x tree
                                    EQ => orig
                                    GT => insert x tree1
