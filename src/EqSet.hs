-- |

module EqSet where


import qualified LogicSymbols as LS
import qualified Data.Set as Set
import Debug.Trace (trace)

substitute :: LS.Equal -> Set.Set LS.Equal -> [(LS.Equal,Bool)]
substitute eq@(LS.Equal (LS.VarT _) (LS.Term _ _)) s = map (\eq0 -> case eq0 of
                                                                      (LS.Equal (LS.VarT _) (LS.VarT _)) -> (eq0,False)
                                                                      (LS.Equal t_l t_r) -> let new_eq = LS.Equal (LS.sub eq t_l) (LS.sub eq t_r) in
                                                                                              if eq0 == new_eq then (eq0,False) else (new_eq,True)) $ Set.elems s
-- substitute eq@(LS.Equal (LS.VarT _) (LS.Term _ _)) s = trace ("eq " ++ show eq ++ "before " ++ show s ++ " after " ++ show foo) foo
--   where foo = map (\eq0 -> case eq0 of
--                                                              (LS.Equal (LS.VarT _) (LS.VarT _)) -> (eq0,False)
--                                                              (LS.Equal t_l t_r) -> let new_eq = LS.Equal (LS.sub eq t_l) (LS.sub eq t_r) in
--                                                                                   if eq0 == new_eq then (eq0,False) else (new_eq,True)) $ Set.elems s
substitute _ s = map (\x -> (x,False)) $ Set.elems s
