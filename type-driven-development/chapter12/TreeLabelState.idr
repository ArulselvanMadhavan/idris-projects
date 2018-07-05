module TreeLabelState

import Control.Monad.State
import TreeLabel

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right)
  = do left_t <- treeLabelWith left
       (this :: rest) <- get
       put rest
       right_t <- treeLabelWith right
       pure (Node left_t (this, val) right_t)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = evalState (treeLabelWith tree) [1..]
