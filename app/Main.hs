module Main where

import LogicSymbols
import Unification
import Data.Set as Set
import Form
import Data.Tree (drawTree)
import Data.Functor.Foldable


constC = VarT "C"
constK = VarT "K"

-- constC = Term "C" []
-- constK = Term "K" []
g_of_cx = Term "g" [constC, VarT "x"]
f_of_g_of_cx = Term "f" [g_of_cx]
h_of_z = Term "h" [VarT "x"]
g_of_h_of_z_k = Term "g" [h_of_z, constK]
h_of_yk = Term "h" [VarT "y", constK]
f_of_g_of_yk = Term "f" [g_of_h_of_z_k]

eq = Equal f_of_g_of_cx f_of_g_of_yk

pred_p = Pred "P" [h_of_z, g_of_cx]
pred_p' = Pred "P" [constC, g_of_cx, constK]
pred_q = Pred "Q" [h_of_yk, constK]

(result, emptyValues) = span (\(_, candidates, _) -> (not.(Set.null)) candidates) (unifyRaw [eq])

computation = concat [result,[head emptyValues]]

(setFinal, candidatesFinal, _) = head emptyValues

-- pred_p = Pred "P" [g_of_cx, h_of_z]
-- pred_q = Pred "Q" [h_of_yk, constK]
-- lit_p = Fix (Qfree.Ltr (Pos pred_p))
-- lit_q = Fix (Qfree.Ltr (Neg pred_q))
-- lit_p' = Fix (QF.Ltr (Pos pred_p))
-- lit_q' = Fix (QF.Ltr (Neg pred_q))
-- randomFormula = Fix (Qfree.Or [lit_p,lit_q])
-- randomFormula3 = Fix (Qfree.And [lit_p,lit_q])
-- randomFormulaFinal = Fix (Qfree.And [randomFormula3,randomFormula3, randomFormula])
-- randomFormula2 = Fix (QF.Qtfy [QF.Forall (Var "x")] (Fix (QF.And [lit_p',lit_q'])))

pred1 = Pred "P1" []
pred2 = Pred "P2" []
pred3 = Pred "P3" []
pred4 = Pred "P4" []
pred5 = Pred "P5" []
pred6 = Pred "P6" []
pred7 = Pred "P7" []
pred8 = Pred "P8" []

lit1 = Fix (Ltr (Pos pred1))
lit2 = Fix (Ltr (Neg pred2))
lit3 = Fix (Ltr (Pos pred3))
lit4 = Fix (Ltr (Neg pred4))
lit5 = Fix (Ltr (Pos pred5))
lit6 = Fix (Ltr (Neg pred6))
lit7 = Fix (Ltr (Pos pred7))
lit8 = Fix (Ltr (Neg pred8))

clause11 = Fix (Or [lit1, lit2])
clause12 = Fix (Or [lit3, lit4])

clause21 = Fix (Or [lit5, lit6])
clause22 = Fix (Or [lit7, lit8])

formula = Fix (Or [(Fix (And [clause11,clause12])), (Fix (And [clause21,clause22]))])

main :: IO ()
main = do
        putStrLn "Test main"
        putStrLn (showEqual eq)
        putStrLn (show setFinal)
        putStrLn (show $ zip [1..] $ Prelude.map (\(x,_,_) -> x) computation)
        putStrLn (show $ zip [1..] $ Prelude.map (\(_,y,_) -> y) computation)
        putStrLn (show $ zip [0..] $ Prelude.map (\(_,_,z) -> z) computation)
        putStrLn (show $ cata toCNF formula)
