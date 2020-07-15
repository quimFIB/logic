-- |

module UnifAlg where
import qualified LogicSymbols as LS
import qualified EqSet as ES
import qualified Data.Set as Set
import qualified Data.Map as Map
import UniSet

data Case = Var2Var | Term2Var | Var2Term | Term2Term

showCase :: Case -> String
showCase Var2Var = "Var2Var"
showCase Term2Var = "Term2Var"
showCase Var2Term = "Var2Term"
showCase Term2Term = "Term2Term"

data UnifyStep = Del | Flip | Fail | Substitute | Decompose | Eliminate

data Log = Log UnifyStep LS.Equal Case | Initial

showLog :: Log -> String
showLog (Log Del eq c) = "[Del;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog (Log Flip eq c) = "[Flip;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog (Log Fail eq c) = "[Fail;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog (Log Substitute eq c) = "[Substitute;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog (Log Decompose eq c) = "[Decompose;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog (Log Eliminate eq c) = "[Eliminate;" ++ LS.showEqual eq ++ ";" ++ showCase c ++ "]"
showLog Initial = "[Init]"

instance Show Log where
  show = showLog

eliminate :: LS.Equal -> Set.Set LS.Equal -> Set.Set LS.Equal
eliminate eq s = Set.insert eq $ Set.map (\(LS.Equal t_l t_r) -> LS.Equal (LS.sub eq t_l) (LS.sub eq t_r)) s

occursCheck :: LS.Equal -> Bool
occursCheck (LS.Equal (LS.VarT x) t) = LS.occurs (LS.Var x) t
occursCheck _ = False

conflict :: LS.Equal -> Bool
conflict (LS.Equal t_l t_r) = LS.conflict t_l t_r

unifyStep :: (UniSet, Set.Set LS.Equal, LS.Equal) -> (UniSet, Set.Set LS.Equal, Log)
unifyStep (set, candidates, eq0@(LS.Equal (LS.VarT x) (LS.VarT y))) =  (UniSet.substitute eq0 set, Set.delete eq0 candidates, (Log Substitute eq0 Var2Var))

unifyStep (set, candidates, eq0@(LS.Equal (LS.Term f l) (LS.VarT x))) = (UniSet { vSet = (UniSet.vSet set), eqSet = Set.insert (LS.reflex eq0) (Set.delete eq0 (UniSet.eqSet set)) },
                                                                          (Set.insert (LS.reflex eq0) (Set.delete eq0 (UniSet.eqSet set))), (Log Flip eq0 Var2Var))

unifyStep (set, candidates, eq0@(LS.Equal (LS.VarT x) t@(LS.Term f l))) = let {contains = UniSet.contains (LS.Var x) set; occurs = occursCheck eq0} in
                                                                             if contains &&  not occurs then let subs_set = UniSet.substitute eq0 (UniSet.delete eq0 set) in
                                                                               (UniSet.insert eq0 subs_set, Set.fromList $ map fst $ ES.substitute eq0 (Set.delete eq0 candidates), Log Eliminate eq0 Var2Term)
                                                                             else if not contains then (set, Set.delete eq0 candidates, Log Del eq0 Var2Term)
                                                                             else (UniSet.empty, Set.empty, Log Fail eq0 Term2Term)
unifyStep (set, candidates, eq0@(LS.Equal t_l t_r))
  | t_l == t_r = (UniSet.delete eq0 set, Set.delete eq0 candidates, Log Del eq0 Term2Term)
  | LS.decomposable eq0 = let new_eqs_list = LS.decompose eq0 in (UniSet.union (UniSet.fromList new_eqs_list) (UniSet.delete eq0 set),
                                                                   Set.union (Set.delete eq0 candidates) (Set.fromList new_eqs_list),
                                                                   Log Decompose eq0 Term2Term)
  | otherwise = (UniSet.empty, Set.empty, Log Fail eq0 Term2Term)

unifyHeader :: (Set.Set LS.Equal -> LS.Equal) -> (UniSet, Set.Set LS.Equal, Log) -> (UniSet, Set.Set LS.Equal, Log)
unifyHeader choose (set, candidates, _) = unifyStep (set,candidates,(choose candidates))

unify :: [LS.Equal] -> [(UniSet, Set.Set LS.Equal, Log)]
unify l = iterate (unifyHeader (Set.elemAt 0)) (set0, candidates0, Initial)
  where set0 = UniSet.fromList l
        candidates0 = Set.fromList l
