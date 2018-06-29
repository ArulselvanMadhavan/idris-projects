module Ex11_2_1

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

public export
data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More x) (Do y f) = do c <- y
                           run x (f c)

export partial
forever : Fuel
forever = More forever

total
totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt f = do putStr prompt
                        input <- getLine
                        totalREPL (f input) f

main : IO ()
main = run forever (totalREPL "\n: " toUpper)
