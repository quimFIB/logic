module Main where

import LogicSymbols
import UnifAlg
import Data.Set as Set
import UniSet
import QfreeForm as Qfree
import Data.Fix


constC = VarT "C"
constK = VarT "K"
g_of_cx = Term "g" [constC, VarT "x"]
f_of_g_of_cx = Term "f" [g_of_cx]
h_of_z = Term "h" [VarT "x"]
g_of_h_of_z_k = Term "g" [h_of_z, constK]
h_of_yk = Term "h" [VarT "y", constK]
f_of_g_of_yk = Term "f" [g_of_h_of_z_k]

eq = Equal f_of_g_of_cx f_of_g_of_yk

pred_p = Pred "P" [g_of_cx, h_of_z]
pred_q = Pred "Q" [h_of_yk, constK]
lit_p = Fix (Qfree.Ltr (Pos pred_p))
lit_q = Fix (Qfree.Ltr (Neg pred_q))
randomFormula = Fix (Qfree.And lit_p lit_q)

(result, emptyValues) = span (\(_, candidates, _) -> (not.(Set.null)) candidates) (unify [eq])

computation = concat [result,[head emptyValues]]

(setFinal, candidatesFinal, _) = head emptyValues

main :: IO ()
main = do
        putStrLn "Test main"
        putStrLn (showEqual eq)
        putStrLn (show setFinal)
        putStrLn (show $ Prelude.map (\(x,_,_) -> x) computation)
        putStrLn (show $ Prelude.map (\(_,y,_) -> y) computation)
        putStrLn (show $ Prelude.map (\(_,_,z) -> z) computation)
        putStrLn (show $ cata Qfree.toTree randomFormula)
