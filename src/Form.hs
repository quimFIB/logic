-- |

module Form where
import PropSymbols
import Data.Fix

data Atom = Pred String [Term]
data Literal a = Pos a | Neg a

-- I don't know what I am doing LMAO
data FormF a = Atm Atom
            | And (FormF a) (FormF a)
            | Or (FormF a) (FormF a)
            | Not (FormF a)
            | Forall Var (FormF a)
            | Exists Var (FormF a)
type Form = Fix FormF

-- neg :: Form -> Form
-- neg (Fix (And f0 f1)) = (Fix fix (Or (neg f0) (neg f1)))
