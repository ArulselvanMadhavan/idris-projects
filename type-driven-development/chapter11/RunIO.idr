module RunIO

public export
data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do   : IO a -> (a -> Inf (RunIO b)) -> RunIO b

export
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do
