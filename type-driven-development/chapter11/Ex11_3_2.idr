module Ex11_3_2

||| Supported Input commands
public export
data Command : Type -> Type where
     PutStr  : (output: String) -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : String -> String -> Command (Either FileError ())
     Pure    : ty -> Command ty
     Bind    : Command a -> (a -> Command b) -> Command b
