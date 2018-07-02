module ArithCmdDo

import ArithCmd

public export
data Input = Answer Int | QuitCmd

export
readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

export
quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
quiz (num1 :: (num2 :: nums)) score =
  do PutStr ("Score so far: " ++ show score ++ "\n")
     input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
     case input of
          Answer answer => if answer == num1 * num2
                              then correct nums score
                              else wrong nums (num1 * num2) score
          QuitCmd => Quit score
