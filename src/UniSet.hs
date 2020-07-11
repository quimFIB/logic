-- |

module UniSet where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified PropSymbols as PS
import qualified EqSet as ES

data UniSet = UniSet {vSet :: Map.Map PS.Var Int, eqSet :: Set.Set PS.Equal}

union :: UniSet -> UniSet -> UniSet
union s0 s1 = UniSet {vSet = Map.union (vSet s0) (vSet s0), eqSet = Set.union (eqSet s0) (eqSet s1)}

add :: PS.Equal -> UniSet -> UniSet
add eq s = let found = Set.member eq (eqSet s) in if not found then UniSet {vSet = Map.union (vSet s) (PS.getVarsEq eq), eqSet = Set.insert eq (eqSet s)} else s

remove :: PS.Equal -> UniSet -> UniSet
remove eq s = let found = Set.member eq (eqSet s) in if found then UniSet {vSet = Map.differenceWith (\x y -> if x < y then Nothing else Just (x - y)) (vSet s) (PS.getVarsEq eq) , eqSet = Set.delete eq (eqSet s)} else s

contains :: PS.Var -> UniSet -> Bool
contains v s = Map.member v (vSet s)

empty :: UniSet
empty = UniSet {vSet = Map.empty, eqSet = Set.empty}

substitute :: PS.Equal -> UniSet -> UniSet
substitute eq@(PS.Equal (PS.VarT _) t) s = let {modifications = map fst $ filter snd substitutions;
                                                 new_vars = PS.getVarsTerm t} in
                                             let new_vars_last = Map.map (\x -> x + (Map.size) new_vars * (length modifications)) new_vars in
                                               UniSet {vSet = Map.union (vSet s) new_vars_last, eqSet = Set.fromList $ map fst substitutions}
                                               where substitutions = (ES.substitute eq (eqSet s))

substitute _ _ = empty

fromList :: [PS.Equal] -> UniSet
fromList = foldl (flip add) empty

showEqs :: UniSet -> String
showEqs s = show (eqSet s)

instance Show UniSet where
  show = showEqs
