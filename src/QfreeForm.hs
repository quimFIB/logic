-- |

module QfreeForm where
import qualified LogicSymbols as LS
import Data.Fix
import Common
-- type Algebra f a = f a -> a
-- I don't know what I am doing LMAO
data FormF a = Ltr (LS.Negation LS.Atom)
            | And a a
            | Or a a
            -- | Not a

type Form = Fix FormF

instance Functor FormF  where
  fmap eval (Ltr l) = Ltr l
  fmap eval (And p q) = And (eval p) (eval q)
  fmap eval (Or p q) = Or (eval p) (eval q)
-- type FormStr = Algebra FormF String

showForm :: FormF String -> String
showForm (Ltr l) = show l
showForm (And s0 s1) = "(" ++ s0 ++ ")" ++ " and " ++ "(" ++ s1 ++ ")"
showForm (Or s0 s1) = "(" ++ s0 ++ ")" ++ " or " ++ "(" ++ s1 ++ ")"
-- showForm (Not s) = "¬" ++ s

toTree :: FormF (Tree String) -> Tree String
toTree (Ltr t) = Leaf (show t)
toTree (And t0 t1) = Twice "and" t0 t1
toTree (Or t0 t1) = Twice "or" t0 t1

neg :: FormF Form -> Form
neg (Ltr l) = Fix (Ltr (LS.flipN l))
neg (And f0 f1) = Fix (Or f0 f1)
neg (Or f0 f1) = Fix (And f0 f1)
