-- |

module UniSet where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified LogicSymbols as LS
import qualified EqSet as ES

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
                                             let new_vars_last = Map.map (\x -> x + (Map.size) new_vars * (length modifications)) new_vars in
                                               UniSet {vSet = Map.union (vSet s) new_vars_last, eqSet = Set.fromList $ map fst substitutions}
                                               where substitutions = (ES.substitute eq (eqSet s))

substitute _ _ = empty

fromList :: [LS.Equal] -> UniSet
fromList = foldl (flip insert) empty

showEqs :: UniSet -> String
showEqs s = show (eqSet s)

instance Show UniSet where
  show = showEqs
