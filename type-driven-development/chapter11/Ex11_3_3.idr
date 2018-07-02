module Ex11_3_3

import Ex11_3_2
import Data.Vect

||| RunCommand samples
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile x) = readFile x
runCommand (WriteFile filePath x) = writeFile filePath x
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand $ f res

data ShellCommand : Type  where
     Cat : (fileName : String) -> ShellCommand
     Copy : (src : String) -> (dest : String) -> ShellCommand
     Exit : ShellCommand

data Fuel : Type where
     Dry  : Fuel
     More : (fuel : Lazy Fuel) -> Fuel

forever : Fuel
forever = More forever

fromList'' : Vect len String -> (l : List String) -> Vect (length l + len) String
fromList'' ys [] = ys
fromList'' {len} ys (x :: xs) =
  rewrite plusSuccRightSucc (length xs) len in
    (fromList'' (x :: ys) xs)

fromListStr : (xs : List String) -> Vect (length xs) String
fromListStr xs = rewrite sym (plusZeroRightNeutral (length xs))
                      in (fromList'' [] xs)

namespace CommandDo
  (>>=) : Command a -> (a -> Inf (Command b)) -> Command b

toShellCommand : (cmd : String) -> (words : List String) -> Maybe ShellCommand
toShellCommand "cat" (x :: []) = Just (Cat x)
toShellCommand "copy" (src :: (dest :: [])) = Just (Copy src dest)
toShellCommand "exit" _ = Just Exit
toShellCommand _ _ = Nothing

validateInput : (words : List String) -> Maybe (String, List String)
validateInput words with (nonEmpty words)
  validateInput (x :: xs) | (Yes IsNonEmpty) = Just (x, xs)
  validateInput words | (No contra) = Nothing


|||RunShell takes fuel and works on a stream of a commands to produce
|||IO actions.
processCat : (fileName : String) -> Command ()
processCat fileName = do eContent <- ReadFile fileName
                         case eContent of
                              (Left l) => PutStr (show l)
                              (Right r) => PutStr r

processWrite : (r : String) -> (dest : String) -> Command ()
processWrite r dest = do eRes <- WriteFile r dest
                         case eRes of
                              (Left l) => PutStr (show l)
                              (Right r) => Pure r


processCopy : (src : String) -> (dest : String) -> Command ()
processCopy src dest = do eContent <- ReadFile src
                          case eContent of
                               (Left l) => PutStr (show l)
                               (Right r) => processWrite r dest

runShell : Fuel -> Command ()
runShell Dry = PutStr "Fuel dried. Killing Shell..."
runShell (More fuel) = do line <- GetLine
                          let wordsList = words line
                          let Just (command, inputs) = validateInput wordsList | Nothing => PutStr "Invalid command" -- Problem. This will end the program.
                          case toShellCommand command inputs of
                               Nothing => do PutStr "Not a valid shell command"
                                             runShell fuel
                               (Just (Cat fileName)) => do (processCat fileName)
                                                           runShell fuel
                               (Just (Copy src dest)) => do (processCopy src dest)
                                                            runShell fuel
                               (Just Exit) => do PutStr "Exiting shell..."
                                                 runShell Dry



startShell : Command ()
startShell = do PutStr "Arul's shell Starting up..."
                runShell forever
