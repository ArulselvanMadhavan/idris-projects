module Ex11_1_4

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx + (number / approx)) / 2
                                   in
                                   approx :: square_root_approx number next

check_bounds : (number : Double) -> (bound : Double) -> (value : Double) -> Ordering
check_bounds number bound value =
  compare (abs (number - (value * value))) bound

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) =
  case check_bounds number bound value of
       LT => value
       EQ => value
       GT => square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.000000000001 (square_root_approx number number)

main : IO ()
main = do
  putStrLn . show $ take 3 (square_root_approx 10 10)
  putStrLn . show $ take 3 (square_root_approx 100 25)
  putStrLn . show $ square_root 2500
  putStrLn . show $ square_root 6
  putStrLn . show $ square_root 2501
  pure ()
  -- putStrLn $ take 3 (square_root_approx 100 25)
