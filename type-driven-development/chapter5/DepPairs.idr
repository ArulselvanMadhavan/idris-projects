module DepPairs

import Data.Vect
import ReadVect

-- exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-- exactLength len input = ?exactLength_rhs

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector(blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector(blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn (zip vec1 vec2')
