module Ex11_3_1

import ArithCmd
import ArithCmdDo

mutual
  correct : Stream Int -> (tot : Nat) -> (Score : Nat) -> ConsoleIO Nat
  correct nums tot score = do PutStr "Correct\n"
                              quiz nums tot (score + 1)

  wrong : Stream Int -> Int -> (tot : Nat) -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans tot score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                quiz nums tot score

  quiz : Stream Int -> (tot: Nat) -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: (num2 :: nums)) tot score =
    do PutStr ("Score so far: " ++ show score ++ "\\" ++ show tot ++ "\n")
       input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
       case input of
            Answer answer => if answer == num1 * num2
                                then correct nums tot score
                                else wrong nums (num1 * num2) tot score
            QuitCmd => Quit score
