{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module QForm where
import qualified LogicSymbols as LS
import qualified QfreeForm as Qfree
-- import Data.Fix
import Data.Functor.Foldable
import Data.List (intercalate)
import Data.Tree

data Quantifier a = Forall a | Exists a


showQtfier :: Show a => Quantifier a -> String
showQtfier (Forall a) = "QF " ++ show a
showQtfier (Exists a) = "QE " ++ show a

instance Show a => Show (Quantifier a) where
  show = showQtfier

type VarQtfy = Quantifier LS.Var

flipQtfier :: VarQtfy -> VarQtfy
flipQtfier (Forall x) = Exists x
flipQtfier (Exists x) = Forall x

data FormF a = Ltr (LS.Negation LS.Atom)
            | And [a]
            | Or [a]
            -- | Not a
            | Qtfy [VarQtfy] a

type Form = Fix FormF

instance Functor FormF  where
  fmap eval (Ltr l) = Ltr l
  fmap eval (And lst) = And $ fmap eval lst
  fmap eval (Or lst) = Or $ fmap eval lst
  fmap eval (Qtfy qlist p) = Qtfy qlist (eval p)

neg :: FormF Form -> Form
neg (Ltr l) = Fix (Ltr (LS.flipN l))
neg (And lst) = Fix (Or lst)
neg (Or lst) = Fix (And lst)
neg (Qtfy qlist f) = Fix (Qtfy (map flipQtfier qlist) f)

showForm :: FormF String -> String
showForm (Ltr lit) = show lit
showForm (And lst) = "(" ++ intercalate " and " lst ++ ")"
showForm (Or lst) = "(" ++ intercalate " or " lst ++ ")"
showForm (Qtfy qlist s) =  "(" ++ concatMap show qlist ++ ".(" ++ show s ++ ")"
-- showForm (Not s) = "¬" ++ s

toTree :: FormF (Tree String) -> Tree String
toTree (Ltr t) = Node (show t) []
toTree (And lst) = Node "and" lst
toTree (Or lst) = Node "or" lst
toTree (Qtfy qlist t) = Node (concatMap show qlist) [t]

flatAnd :: FormF Form -> [Form]
flatAnd (And lst) = lst
flatAnd f = [Fix f]

flatOr :: FormF Form -> [Form]
flatOr (Or lst) = lst
flatOr f = [Fix f]

flat :: FormF Form -> Form
flat (Ltr l) = Fix (Ltr l)
flat (And lst) = Fix (And (concatMap (flatAnd . unfix) lst))
flat (Or lst) = Fix (Or (concatMap (flatOr . unfix) lst))
flat (Qtfy qlist f) = Fix (Qtfy qlist f)

-- Start Prenex stuff
data PrenexForm = Prenex [VarQtfy] Qfree.Form

-- We assume that there no repeated variables in the formula
-- prenex :: FormF PrenexForm -> PrenexForm
-- prenex (Ltr l) = Prenex [] (Fix (Qfree.Ltr l))
-- prenex (And (Prenex l0 f0) (Prenex l1 f1)) = Prenex (l0 ++ l1) (Fix (Qfree.And f0 f1))
-- prenex (Or (Prenex l0 f0) (Prenex l1 f1)) = Prenex (l0 ++ l1) (Fix (Qfree.Or f0 f1))
-- prenex (Qtfy qlist (Prenex q'list f)) = Prenex (qlist ++ q'list) f
