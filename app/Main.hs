module Main where

import LogicSymbols
import UnifAlg
import Data.Set as Set
import UniSet
import qualified QfreeForm as Qfree
import qualified QForm as QF
import Data.Fix
import Data.Tree (drawTree)


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
lit_p' = Fix (QF.Ltr (Pos pred_p))
lit_q' = Fix (QF.Ltr (Neg pred_q))
randomFormula = Fix (Qfree.And [lit_p,lit_q])
randomFormula3 = Fix (Qfree.Or [lit_p,lit_q])
randomFormulaFinal = Fix (Qfree.And [randomFormula3,randomFormula3])
randomFormula2 = Fix (QF.Qtfy [QF.Forall (Var "x")] (Fix (QF.And [lit_p',lit_q'])))

(result, emptyValues) = span (\(_, candidates, _) -> (not.(Set.null)) candidates) (unify [eq])

computation = concat [result,[head emptyValues]]

(setFinal, candidatesFinal, _) = head emptyValues

main :: IO ()
main = do
        -- putStrLn "Test main"
        -- putStrLn (showEqual eq)
        -- putStrLn (show setFinal)
        -- putStrLn (show $ Prelude.map (\(x,_,_) -> x) computation)
        -- putStrLn (show $ Prelude.map (\(_,y,_) -> y) computation)
        -- putStrLn (show $ Prelude.map (\(_,_,z) -> z) computation)
        -- putStrLn (show $ cata Qfree.toTree $ cata (Qfree.distribute randomFormula3 Qfree.makeOr) randomFormulaFinal)
        putStrLn (drawTree $ cata Qfree.toTree randomFormula)
        -- putStrLn (show $ cata QF.toTree $ (cata QF.neg randomFormula2))
