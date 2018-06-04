module ReadVect

import Data.Vect

%access export


data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

readVect' : IO (VectUnknown String)
readVect' = do x <- getLine
               if (x == "")
               then pure (MkVect _ [])
               else do MkVect _ xs <- readVect'
                       pure (MkVect _ (x :: xs))

readVect : IO (p ** Vect p String)
readVect = do x <- getLine
              if (x == "")
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure (_ ** (x :: xs))
