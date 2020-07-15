-- |

module EqSet where


import qualified LogicSymbols as LS
import qualified Data.Set as Set

substitute :: LS.Equal -> Set.Set LS.Equal -> [(LS.Equal,Bool)]
substitute (LS.Equal (LS.VarT _) (LS.Term _ _)) s = map (\eq -> case eq of
                                                             (LS.Equal (LS.VarT _) (LS.VarT _)) -> (eq,False)
                                                             (LS.Equal t_l t_r) -> let new_eq = LS.Equal (LS.sub eq t_l) (LS.sub eq t_r) in
                                                                                  if eq == new_eq then (eq,False) else (new_eq,True)) $ Set.elems s
substitute _ s = map (\x -> (x,False)) $ Set.elems s
