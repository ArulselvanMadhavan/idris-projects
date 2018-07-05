module Greet

import Ex11_2_1
import RunIO

total
greet : InfIO
greet = do putStr "Enter your name: "
           name <- getLine
           putStrLn ("Hello " ++ name)
           greet

total
greet' : RunIO ()
greet' = do putStr "Enter your name: "
            name <- getLine
            if name == ""
               then do putStrLn "Bye bye!"
                       Quit ()
               else do putStrLn ("Hello " ++ name)
                       greet'
total
run : Fuel -> RunIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run (More _) (Quit x) = pure (Just x)
run (More fuel) (Do y f) = do z <- y
                              run fuel (f z)

partial
main : IO ()
main = do run forever greet'
          pure ()
