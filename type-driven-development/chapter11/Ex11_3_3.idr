module Ex11_3_3

import Ex11_3_2
import Data.Vect

data ShellCommand : Type  where
     Cat : (fileName : String) -> ShellCommand
     Copy : (src : String) -> (dest : String) -> ShellCommand
     Exit : ShellCommand

data Fuel : Type where
     Dry  : Fuel
     More : (fuel : Lazy Fuel) -> Fuel

data ShellIO : Type -> Type where
     Quit : a -> ShellIO a
     Do : Command a -> (a -> Inf (ShellIO b)) -> ShellIO b

namespace ShellDo
  (>>=) : Command a -> (a -> Inf (ShellIO b)) -> ShellIO b
  (>>=) = Do

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

partial
forever : Fuel
forever = More forever

||| RunCommand samples
total
runCommand : Command a -> IO a
runCommand (PutStr x) = putStrLn x
runCommand GetLine = getLine
runCommand (ReadFile x) = readFile x
runCommand (WriteFile filePath x) = writeFile filePath x
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand $ f res

total
fromList'' : Vect len String -> (l : List String) -> Vect (length l + len) String
fromList'' ys [] = ys
fromList'' {len} ys (x :: xs) =
  rewrite plusSuccRightSucc (length xs) len in
    (fromList'' (x :: ys) xs)

total
fromListStr : (xs : List String) -> Vect (length xs) String
fromListStr xs = rewrite sym (plusZeroRightNeutral (length xs))
                      in (fromList'' [] xs)

total
toShellCommand : (cmd : String) -> (words : List String) -> Maybe ShellCommand
toShellCommand "cat" (x :: []) = Just (Cat x)
toShellCommand "copy" (src :: (dest :: [])) = Just (Copy src dest)
toShellCommand "exit" _ = Just Exit
toShellCommand _ _ = Nothing

total
validateInput : (words : List String) -> Maybe (String, List String)
validateInput words with (nonEmpty words)
  validateInput (x :: xs) | (Yes IsNonEmpty) = Just (x, xs)
  validateInput words | (No contra) = Nothing

|||RunShell takes fuel and works on a stream of a commands to produce
|||IO actions.
total
processCat : (fileName : String) -> Command ()
processCat fileName = do eContent <- ReadFile fileName
                         case eContent of
                              (Left l) => PutStr (show l)
                              (Right r) => PutStr r

total
processWrite : (r : String) -> (dest : String) -> Command ()
processWrite r dest = do eRes <- WriteFile dest r
                         case eRes of
                              (Left l) => PutStr ("error in writing file" ++ (show l))
                              (Right r) => Pure r

total
processCopy : (src : String) -> (dest : String) -> Command ()
processCopy src dest = do eContent <- ReadFile src
                          case eContent of
                               (Left l) => PutStr ("error in reading file" ++ (show l))
                               (Right r) => processWrite r dest

total
shell : ShellIO String
shell = do PutStr ">"
           line <- GetLine
           let wordsList = words line
           let Just (command, inputs) = validateInput wordsList | Nothing => do PutStr "Invalid shell command"
                                                                                shell
           case toShellCommand command inputs of
                Nothing => do PutStr "Not a valid shell command"
                              shell
                (Just (Cat fileName)) => do processCat fileName
                                            shell
                (Just (Copy src dest)) => do processCopy src dest
                                             shell
                (Just Exit) => do PutStr "Exiting shell"
                                  Quit "Bye"

||| A generic run function for running ShellIO type
total
run : (fuel : Fuel) -> ShellIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run (More _) (Quit res) = pure (Just res)
run (More fuel) (Do cmd f) = do x <- runCommand cmd
                                run fuel (f x)

partial
main : IO ()
main = do putStrLn "Welcome to Arul's shell"
          Just output <- run forever shell | Nothing => putStrLn "Ran out of fuel"
          putStrLn output
