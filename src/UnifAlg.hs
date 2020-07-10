-- |

module UnifAlg where

import qualified PropSymbols as PS
import qualified Data.Set as Set

substitute :: PS.Equal -> Set.Set PS.Equal -> [(PS.Equal,Bool)]
substitute (PS.Equal (PS.VarT _) (PS.Term _ _)) s = map (\eq -> case eq of
                                                             (PS.Equal (PS.VarT _) (PS.VarT _)) -> (eq,False)
                                                             (PS.Equal t_l t_r) -> let new_eq = PS.Equal (PS.sub eq t_l) (PS.sub eq t_r) in
                                                                                  if eq == new_eq then (eq,False) else (new_eq,True)) $ Set.elems s
substitute _ s = map (\x -> (x,False)) $ Set.elems s
