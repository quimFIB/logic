-- |

module Resolution where

import Unification
import LogicSymbols
import Form
import Data.Set

unifiable :: Literal -> Literal -> Bool
unifiable (Pos (Pred p p_terms)) (Neg (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
unifiable (Neg (Pred p p_terms)) (Pos (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
unifiable _ _ = False

candidates :: Clause -> Clause -> [(Literal,Literal)]
candidates (Clause c0) (Clause c1) = [(x,y) | x <- elems c0, y <- elems c1, unifiable x y]

-- factor :: Literal -> Literal ->

-- resolve :: Clause -> Clause -> Clause
-- resolve c0 c1 =
