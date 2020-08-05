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

factor :: Literal -> Literal -> Maybe Substitution
factor ltr0 ltr1 = let Pred _ trmList0 = getNegated ltr0
                       Pred _ trmList1 = getNegated ltr1
                       equalities = zipWith Equal trmList0 trmList1
                       uniset = unify equalities in
                     ((Sub . substitutionMap) <$> toMap uniset)

-- resolve :: Clause -> Clause -> Clause
-- resolve c0 c1 =
