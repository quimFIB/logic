module Lib
    ( someFunc,
      Var,
      showVar,
      Term,
      showTerm,
      reflex
    ) where

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Data.List (intercalate)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Var = Var String deriving (Eq, Ord)

showVar :: Var -> String
showVar (Var s) = s

var2term :: Var -> Term
var2term (Var s) = VarT s

data Term = VarT String
          | Term String [Term]
          deriving (Eq, Ord)

term2var :: Term -> Maybe Var
term2var t = case t of
               VarT s -> Just (Var s)
               _ -> Nothing

showTerm :: Term -> String
showTerm (VarT s) = s
showTerm (Term s l) = s ++ "(" ++ intercalate "," (map showTerm l) ++ ")"
data Equal = Equal Term Term deriving (Eq, Ord)

reflex :: Equal -> Equal
reflex (Equal t0 t1) = Equal t1 t0

sub :: Equal -> Term -> Term
sub eq@(Equal t_l t_r) t = if t_l == t then t_r else
                           case t of
                             Term s l -> Term s (map (sub eq) l)
                             _ -> t
occurs :: Var -> Term -> Bool
occurs v (Term _ l) = or (map (occurs v) l)
occurs v (VarT s) = v == Var s
