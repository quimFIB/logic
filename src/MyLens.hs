-- |

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module MyLens where

type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)

newtype Identity a = Identity { runIdentity::a }
  deriving Functor

newtype Const a b = Const { getConst::a }
  deriving Functor

view :: Lens s a -> s -> a
view ln s = getConst (ln Const s)

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln (Identity . f) s
