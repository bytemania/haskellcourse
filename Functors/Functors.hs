{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Functors where

import Data.Function
import Control.Applicative
import Data.Functor.Contravariant (Contravariant (..))
import Control.Arrow
import qualified Data.Map as M

newtype Predicate a = Predicate {getPredicate :: a -> Bool}

instance Contravariant Predicate where
    contramap g (Predicate p) = Predicate (p . g)

veryOdd :: Predicate Int
veryOdd = contramap (`div` 2) (Predicate odd)

--main :: IO ()
--main = print $ getPredicate veryOdd <$> [0 .. 11]

newtype Const' a b = Const' a
instance Contravariant (Const' a) where
    contramap _ (Const' a) = Const' a

newtype Comparison a = Comparison (a -> a -> Ordering)
instance Contravariant Comparison where
    contramap f (Comparison comp) = Comparison (comp `on` f)

newtype Op b a = Op (a -> b)
instance Contravariant (Op b) where
    contramap g (Op f) = Op (f . g)

class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Bifunctor Either where
    bimap g h = either (Left . g) (Right . h)

instance Bifunctor (,) where
    bimap = (***)

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

lmap :: Profunctor p => (a -> b) -> p b c -> p a c
lmap f = dimap f id

rmap :: Profunctor p => (c -> d) -> p b c -> p b d
rmap = dimap id

data Limits' a b = Limits' {step :: a -> (b,b), check :: a -> a -> Bool}

instance Profunctor Limits' where
    dimap g h (Limits' step check) = Limits' {step = (h *** h) . step . g, check = check `on` g}

type Limits a = Limits' a a

maybeLimit :: a -> Limits a -> Limits (Maybe a)
maybeLimit d = dimap (maybe d id) Just

millionsLimit :: Limits Double -> Limits Double
millionsLimit = dimap (1.0e6 *)(/ 1.0e6)

instance Profunctor (->) where
    dimap g h f = h . f . g

newtype Indexed i a b = Indexed {runIndexed :: i -> a -> b}

class Indexable i p where
    indexed :: p a b -> i -> a -> b

instance Profunctor (Indexed i) where
    dimap g h (Indexed f) = Indexed (dimap g h . f)

instance Indexable i (Indexed i) where
    indexed = runIndexed

instance Indexable i (->) where
    indexed = const

mapIndexable :: Indexable i p => p a b -> M.Map i a -> M.Map i b
mapIndexable = M.mapWithKey . indexed

