module InfIO

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More Fuel

data Fuel' = Dry' | More' (Lazy Fuel)

total
tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

total
loopPrint : String -> InfIO
loopPrint x = Do (putStrLn x) (\_ => loopPrint x)

total
loopPrint' : String -> InfIO
loopPrint' msg = do putStrLn msg
                    loopPrint' msg


run : InfIO -> IO ()
run (Do action f) = do res <- action
                       run (f res)

|||First attempt to make run total with a finite
|||sequence
total
run' : Fuel -> InfIO -> IO ()
run' Dry (Do x f) = putStrLn "Out of Fuel"
run' (More fuel) (Do x f) = do res <- x
                               run' fuel (f res)

forever : Fuel
forever = More forever

main : IO ()
main = run' (tank 5) (loopPrint "Arul")
