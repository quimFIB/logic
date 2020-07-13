-- |

module Common where
import qualified Data.Tree as DT

data Tree a = Leaf a | Twice a (Tree a) (Tree a) | Once a (Tree a)

toDataTree :: Tree a -> DT.Tree a
toDataTree (Leaf a) = DT.Node a []
toDataTree (Twice a t0 t1) = DT.Node a [toDataTree t0, toDataTree t1]
toDataTree (Once a t) = DT.Node a [toDataTree t]

showTree :: Show a => Tree a -> String
showTree t = DT.drawTree $ fmap show (toDataTree t)

instance Show a => Show (Tree a) where
  show = showTree
