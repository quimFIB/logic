-- |

module Resolution where

import Unification
import LogicSymbols
import Form
import Data.Set

resolvable :: Literal -> Literal -> Bool
resolvable (Pos (Pred p p_terms)) (Neg (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
resolvable (Neg (Pred p p_terms)) (Pos (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
resolvable _ _ = False

candidates :: Clause -> Clause -> [(Literal,Literal)]
candidates (Clause c0) (Clause c1) = [(x,y) | x <- elems c0, y <- elems c1, resolvable x y]

factor :: Literal -> Literal -> Maybe Substitution
factor ltr0 ltr1 = let Pred _ trmList0 = getNegated ltr0
                       Pred _ trmList1 = getNegated ltr1
                       equalities = zipWith Equal trmList0 trmList1
                       uniset = unify equalities in
                     ((Sub . substitutionMap) <$> toMap uniset)

-- resolve :: Clause -> Clause -> Clause
-- resolve c0 c1 =
