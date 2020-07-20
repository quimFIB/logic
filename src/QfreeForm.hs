-- |

module QfreeForm where
import qualified LogicSymbols as LS
import Data.Tree
-- import Data.Fix
import Data.Functor.Foldable
import Data.List (intercalate, tails)
-- type Algebra f a = f a -> a
-- I don't know what I am doing LMAO
data FormF a = Ltr (LS.Negation LS.Atom)
            | And [a]
            | Or [a]
            -- | Not a

type Form = Fix FormF

instance Functor FormF  where
  fmap eval (Ltr lit) = Ltr lit
  fmap eval (And lst) = And $ fmap eval lst
  fmap eval (Or lst) = Or $ fmap eval lst
-- type FormStr = Algebra FormF String

showForm :: FormF String -> String
showForm (Ltr lit) = show lit
showForm (And lst) = "(" ++ intercalate " and " lst ++ ")"
showForm (Or lst) = "(" ++ intercalate " or " lst ++ ")"
-- showForm (Not s) = "¬" ++ s

toTree :: FormF (Tree String) -> Tree String
toTree (Ltr t) = Node (show t) []
toTree (And lst) = Node "and" lst
toTree (Or lst) = Node "or" lst

neg :: FormF Form -> Form
neg (Ltr l) = Fix (Ltr (LS.flipN l))
neg (And lst) = Fix (Or lst)
neg (Or lst) = Fix (And lst)

flatAnd :: FormF Form -> [Form]
flatAnd (And lst) = lst
flatAnd f = [Fix f]

flatOr :: FormF Form -> [Form]
flatOr (Or lst) = lst
flatOr f = [Fix f]

flat :: FormF Form -> Form
flat (Ltr l) = Fix (Ltr l)
flat (And lst) = Fix (And (concatMap (flatAnd . unfix) lst))
flat (Or lst) = Fix (Or (concatMap (flatOr . unfix) lst))

-- distribute :: Form -> (Form -> Form -> Form) -> FormF Form -> Form
-- distribute f c (Ltr l)  = c (Fix (Ltr l)) f
-- distribute f c (And f0 f1) = c (distribute f makeOr (unfix f0)) (distribute f makeOr (unfix f1))
-- distribute f c (Or f0 f1) = c (distribute f makeAnd (unfix f0)) (distribute f makeAnd (unfix f1))



-- distribute :: FormF Form -> Form
-- distribute (Ltr l) = Fix (Ltr l)
-- distribute (And lst) = Fix (Or (fmap (cata distribute . pushAnd) candidates))
--   where pushAnd (x:xs) = (Fix (And [x, xs]))
--         candidates = init $ tails lst
-- distribute (Or lst) = Fix (And (fmap (cata distribute . pushOr) candidates))
--   where pushOr x = Fix (Or x)
--         candidates = init $ tails lst

-- toCNF :: FormF Form -> Form
-- toCNF (Ltr l) = Fix (Ltr l)
-- toCNF (And f0 f1) = Fix (And f0 f1)
-- toCNF (Or f0 f1) = Fix (Or )
