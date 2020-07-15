-- |

module QForm where
import qualified LogicSymbols as LS
import qualified QfreeForm as Qfree
import Data.Fix
import Common

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
            | And a a
            | Or a a
            -- | Not a
            | Qtfy [VarQtfy] a

type Form = Fix FormF

instance Functor FormF  where
  fmap eval (Ltr l) = Ltr l
  fmap eval (And p q) = And (eval p) (eval q)
  fmap eval (Or p q) = Or (eval p) (eval q)
  fmap eval (Qtfy qlist p) = Qtfy qlist (eval p)

neg :: FormF Form -> Form
neg (Ltr l) = Fix (Ltr l)
neg (And f0 f1) = Fix (Or f0 f1)
neg (Or f0 f1) = Fix (And f0 f1)
neg (Qtfy qlist f) = Fix (Qtfy (map flipQtfier qlist) f)

showForm :: FormF String -> String
showForm (Ltr l) = show l
showForm (And s0 s1) = "(" ++ s0 ++ ")" ++ " and " ++ "(" ++ s1 ++ ")"
showForm (Or s0 s1) = "(" ++ s0 ++ ")" ++ " or " ++ "(" ++ s1 ++ ")"
showForm (Qtfy qlist s) =  "(" ++ concatMap show qlist ++ ".(" ++ show s ++ ")"
-- showForm (Not s) = "¬" ++ s

toTree :: FormF (Tree String) -> Tree String
toTree (Ltr t) = Leaf (show t)
toTree (And t0 t1) = Twice " and " t0 t1
toTree (Or t0 t1) = Twice " or " t0 t1
toTree (Qtfy qlist t) = Once (concatMap show qlist) t



-- Start Prenex stuff
data PrenexForm = Prenex [VarQtfy] Qfree.Form

discardQualifiers :: PrenexForm -> Qfree.Form
discardQualifiers (Prenex _ f) = f

prenex :: FormF PrenexForm -> PrenexForm
prenex (Ltr l) = Prenex [] (Fix (Qfree.Ltr l))
prenex (And f0 f1) = Prenex [] (Fix (Qfree.Or (discardQualifiers f0) (discardQualifiers f1)))
prenex (Or f0 f1) = Prenex [] (Fix (Qfree.And (discardQualifiers f0) (discardQualifiers f1)))
prenex (Qtfy qlist (Prenex q'list f)) = Prenex (qlist ++ q'list) f
