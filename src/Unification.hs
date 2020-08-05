-- |

module Unification where

import qualified LogicSymbols as LS
import qualified EqSet as ES
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (foldM)

data UniSet = UniSet {vSet :: Map.Map LS.Var Int, eqSet :: Set.Set LS.Equal}

union :: UniSet -> UniSet -> UniSet
union s0 s1 = UniSet {vSet = Map.union (vSet s0) (vSet s0), eqSet = Set.union (eqSet s0) (eqSet s1)}

insert :: LS.Equal -> UniSet -> UniSet
insert eq s = let found = Set.member eq (eqSet s) in if not found then UniSet {vSet = Map.union (vSet s) (LS.getVarsEq eq), eqSet = Set.insert eq (eqSet s)} else s

delete :: LS.Equal -> UniSet -> UniSet
delete eq s = let found = Set.member eq (eqSet s) in if found then UniSet {vSet = Map.differenceWith (\x y -> if x < y then Nothing else Just (x - y)) (vSet s) (LS.getVarsEq eq) , eqSet = Set.delete eq (eqSet s)} else s

contains :: LS.Var -> UniSet -> Bool
contains v s = Map.member v (vSet s)

empty :: UniSet
empty = UniSet {vSet = Map.empty, eqSet = Set.empty}

substitute :: LS.Equal -> UniSet -> UniSet
substitute eq@(LS.Equal (LS.VarT _) t) s = let {modifications = map fst $ filter snd substitutions;
                                                 new_vars = LS.getVarsTerm t} in
                                             let new_vars_last = Map.map (\x -> x + Map.size new_vars * length modifications) new_vars in
                                               UniSet {vSet = Map.union (vSet s) new_vars_last, eqSet = Set.fromList $ map fst substitutions}
                                               where substitutions = ES.substitute eq (eqSet s)

substitute _ _ = empty

toMap :: UniSet -> Maybe (Map.Map LS.Var LS.Term)
toMap uset = foldM (\s p -> case p of {Just (x,t) -> return (Map.insert x t s) ; Nothing -> Nothing}) Map.empty (fmap (\(LS.Equal t0 t1) -> do x <- LS.term2var t0; return (x, t1)) equalities)
  where equalities = Set.toList $ eqSet uset

newtype Substitution = Sub (LS.Term -> LS.Term)

substitutionMap :: Map.Map LS.Var LS.Term -> LS.Term -> LS.Term
substitutionMap m xt@(LS.VarT x) = maybe xt id (Map.lookup (LS.Var x) m)
substitutionMap m (LS.Term t t_list) = LS.Term t (fmap (substitutionMap m) t_list)

fromList :: [LS.Equal] -> UniSet
fromList = foldl (flip insert) empty

showEqs :: UniSet -> String
showEqs s = show (eqSet s)

instance Show UniSet where
  show = showEqs

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
unifyStep (set, candidates, eq0@(LS.Equal (LS.VarT _) (LS.VarT _))) =  (substitute eq0 set, Set.delete eq0 candidates, Log Substitute eq0 Var2Var)

unifyStep (set, _, eq0@(LS.Equal (LS.Term _ _) (LS.VarT _))) = (UniSet { vSet = vSet set, eqSet = Set.insert (LS.reflex eq0) (Set.delete eq0 (eqSet set)) },
                                                                          Set.insert (LS.reflex eq0) (Set.delete eq0 (eqSet set)), Log Flip eq0 Var2Var)

unifyStep (set, candidates, eq0@(LS.Equal (LS.VarT x) (LS.Term _ _))) = let {hasOcurrence = contains (LS.Var x) set; occurs = occursCheck eq0} in
                                                                             if hasOcurrence &&  not occurs then let subs_set = substitute eq0 (delete eq0 set) in
                                                                               (insert eq0 subs_set, Set.fromList $ map fst $ ES.substitute eq0 (Set.delete eq0 candidates), Log Eliminate eq0 Var2Term)
                                                                             else if not hasOcurrence then (set, Set.delete eq0 candidates, Log Del eq0 Var2Term)
                                                                             else (empty, Set.empty, Log Fail eq0 Term2Term)
unifyStep (set, candidates, eq0@(LS.Equal t_l t_r))
  | t_l == t_r = (delete eq0 set, Set.delete eq0 candidates, Log Del eq0 Term2Term)
  | LS.decomposable eq0 = let new_eqs_list = LS.decompose eq0 in (fromList new_eqs_list `union` delete eq0 set,
                                                                   Set.union (Set.delete eq0 candidates) (Set.fromList new_eqs_list),
                                                                   Log Decompose eq0 Term2Term)
  | otherwise = (empty, Set.empty, Log Fail eq0 Term2Term)

unifyHeader :: (Set.Set LS.Equal -> LS.Equal) -> (UniSet, Set.Set LS.Equal, Log) -> (UniSet, Set.Set LS.Equal, Log)
unifyHeader choose (set, candidates, _) = unifyStep (set, candidates, choose candidates)

unifyRaw :: [LS.Equal] -> [(UniSet, Set.Set LS.Equal, Log)]
unifyRaw l = iterate (unifyHeader (Set.elemAt 0)) (set0, candidates0, Initial)
  where set0 = fromList l
        candidates0 = Set.fromList l

unify :: [LS.Equal] -> UniSet
unify l = let (_, result) = span (\(_, candidates, _) -> (not.Set.null) candidates) (unifyRaw l)
              (uniSet, _, _) = head result in uniSet
