{-# LANGUAGE DeriveGeneric #-}

module LogicSymbols where

import GHC.Generics (Generic)
import Data.Hashable


import Data.List (intercalate)
import qualified Data.Map as Map
import MyLens

newtype Var = Var String deriving (Eq, Ord, Show)

showVar :: Var -> String
showVar (Var s) = s

var2term :: Var -> Term
var2term (Var s) = VarT s


data Term = VarT String
          | Term String [Term]
          deriving (Eq, Ord, Generic)
instance Hashable Term

term2var :: Term -> Maybe Var
term2var t = case t of
               VarT s -> Just (Var s)
               _ -> Nothing

showTerm :: Term -> String
showTerm (VarT s) = s
showTerm (Term s l) = s ++ "(" ++ intercalate "," (map showTerm l) ++ ")"

instance Show Term where
  show = showTerm

data Equal = Equal Term Term deriving (Eq, Ord)
showEqual :: Equal -> String
showEqual (Equal t_l t_r) = showTerm t_l ++ " = " ++ showTerm t_r

instance Show Equal where
  show = showEqual

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

conflict :: Term -> Term -> Bool
conflict (Term f l_f) (Term g l_g) = f /= g || length l_f /= length l_g
conflict _ _ = False

decomposable :: Equal -> Bool
decomposable (Equal t_l@(Term _ _) t_r@(Term _ _)) = not $ conflict t_l t_r
decomposable _ = False

decompose :: Equal -> [Equal]
decompose (Equal (Term _ l_l) (Term _ l_r)) =  zipWith (\x y -> Equal x y) l_l l_r
decompose _ = []

getVarsTerm :: Term -> Map.Map Var Int
-- getVarsTerm (VarT s) = Map.insertWith' (+) (Var s) 1 Map.empty
getVarsTerm (VarT s) = Map.insert (Var s) 1 Map.empty
getVarsTerm (Term _ l_f) = Map.unions $ map getVarsTerm l_f

getVarsEq :: Equal -> Map.Map Var Int
getVarsEq (Equal t_l t_r) = Map.unionWith (+) (getVarsTerm t_l) (getVarsTerm t_r)

-- Helpers for FOL

data Atom = Pred String [Term] deriving (Eq, Ord, Generic)

instance Hashable Atom

atomTermsLens :: Lens Atom [Term]
atomTermsLens f (Pred s terms) = Pred s <$> f terms

-- instance Ord Atom where
--   (<=) (Pred s0 t0) (Pred s1 t1) = length t0 < length t1 || s0 < s1 || or (fmap (uncurry (<=)) (zip t0 t1))

showAtom :: Atom -> String
showAtom (Pred str []) =  str
showAtom (Pred str l) =  str ++ "(" ++ intercalate "," (map showTerm l) ++ ")"

instance Show Atom where
  show = showAtom

data Negation a = Pos {getNegated::a} | Neg {getNegated::a} deriving (Eq, Generic)

instance Hashable a => Hashable (Negation a)

negationLens :: Lens (Negation a) a
negationLens f negation = (\a -> negation {getNegated = a}) <$> f (getNegated negation)

instance Ord a => Ord (Negation a) where
  (<=) (Pos _) (Neg _) = False
  (<=) (Neg _) (Pos _) = True
  (<=) (Pos a0) (Pos a1) = a0 <= a1
  (<=) (Neg a0) (Neg a1) = a0 <= a1


showNegation :: Show a => Negation a -> String
showNegation (Pos x) = show x
showNegation (Neg x) = "!" ++ show x

instance Show a => Show (Negation a) where
  show = showNegation

flipN :: Negation a -> Negation a
flipN (Pos x) = Neg x
flipN (Neg x) = Pos x

-- For some reason this type synonym gives quite a few errors???
type Literal = Negation Atom

showLiteral :: Negation Atom -> String
showLiteral (Pos a) = show a
showLiteral (Neg a) = "!" ++ show a

-- instance Show (Negation Atom) where
--   show = showNegation

-- neg :: Literal -> Literal  -- not working???
neg :: Negation Atom -> Negation Atom
neg = flipN
