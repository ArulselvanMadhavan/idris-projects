module Ex11_1_1

every_other : Stream Int -> Stream Int
every_other (y :: (x :: xs)) = x :: (every_other xs)
