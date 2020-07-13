-- |

module Form where
import qualified PropSymbols as PS
import Data.Fix

-- type Algebra f a = f a -> a
-- I don't know what I am doing LMAO
data FormF a = Ltr (PS.Negation PS.Atom)
            | And a a
            | Or a a
            -- | Not a
            | Forall PS.Var a
            | Exists PS.Var a

type Form = Fix FormF

-- type FormStr = Algebra FormF String

showForm :: FormF String -> String
showForm (Ltr l) = show l
showForm (And s0 s1) = s0 ++ "∧" ++ s1
showForm (Or s0 s1) = s0 ++ "∨" ++ s1
showForm (Forall x s) = "∀" ++ show x ++ s
showForm (Exists x s) = "∃" ++ show x ++ s
-- showForm (Not s) = "¬" ++ s

neg :: FormF Form -> Form
neg (Ltr l) = Fix (Ltr l)
neg (And f0 f1) = Fix (Or f0 f1)
neg (Or f0 f1) = Fix (And f0 f1)
neg (Forall x f) = Fix (Exists x f)
neg (Exists x f) = Fix (Forall x f)
