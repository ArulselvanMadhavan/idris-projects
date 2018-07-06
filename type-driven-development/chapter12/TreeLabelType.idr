module TreeLabelType

import TreeLabel

||| Custom State Type
public export
data State : (stateType : Type) -> Type -> Type where
     Get : State stateType stateType
     Put : stateType -> State stateType ()
     Pure : ty -> State stateType ty
     Bind : State stateType a -> (a -> State stateType b) -> State stateType b

get : State stateType stateType
get = Get

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

export
runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put x) st = ((), x)
runState (Pure x) st = (x, st)
runState (Bind prev f) st = let (val, next) = runState prev st
                            in
                            runState (f val) next


treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = Pure Empty
treeLabelWith (Node left val right)
  = do left_l <- treeLabelWith left
       (this :: rest) <- Get
       Put rest
       right_l <- treeLabelWith right
       Pure (Node left_l (this, val) right_l)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel x = fst $ runState (treeLabelWith x) [1..]
