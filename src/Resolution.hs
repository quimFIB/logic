-- |
module Resolution where

import qualified Unification as Unif
import LogicSymbols
import Form
import Data.HashSet
import MyLens

data RClause = RClause {literal::Literal, clause::Clause}


resolvable :: Literal -> Literal -> Bool
resolvable (Pos (Pred p p_terms)) (Neg (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
resolvable (Neg (Pred p p_terms)) (Pos (Pred q q_terms)) = (p == q) && (length p_terms == length q_terms)
resolvable _ _ = False

candidates :: Clause -> Clause -> [(RClause,RClause)]
candidates (Clause c0) (Clause c1) = [(RClause x (Clause c0), RClause y (Clause c1)) | x <- toList c0, y <- toList c1, resolvable x y]

factor :: Literal -> Literal -> Maybe Unif.Substitution
factor ltr0 ltr1 = let Pred _ trmList0 = getNegated ltr0
                       Pred _ trmList1 = getNegated ltr1
                       equalities = zipWith Equal trmList0 trmList1
                       uniset = Unif.unify equalities in
                     ((Unif.Sub . Unif.substitutionMap) <$> Unif.toMap uniset)

resolve :: RClause -> RClause -> (Clause, Unif.Substitution)
resolve c0 c1 = let equalities = zipWith Equal (view (negationLens . atomTermsLens) (literal c0))
                                 (view (negationLens . atomTermsLens) (literal c1))
                    uniset = Unif.unify equalities in
                  (Clause $ union (view clauseLens (clause c0)) (view clauseLens (clause c1)),
                   maybe (Unif.Sub id) id ((Unif.Sub . Unif.substitutionMap) <$> Unif.toMap uniset))

resolutionStep :: (HashSet Clause, [(RClause, RClause)]) -> (HashSet Clause, [(RClause, RClause)])
resolutionStep (clauses, candidatesList) = let resolvents = head candidatesList
                                               (newClause, Unif.Sub subs) = uncurry resolve resolvents
                                               subsClauses = Data.HashSet.map (\(Clause c) -> Clause $ Data.HashSet.map (over (negationLens . atomTermsLens) (Prelude.map subs)) c) clauses in
                                             (subsClauses `union` singleton newClause, candidatesList ++ concat (Prelude.map (candidates newClause) (toList clauses)))
