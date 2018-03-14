{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fold
       (
         Pair(..)
       , NonEmpty(..)
       , splitOn
       , joinWith
       ) where

import Data.Semigroup

data Pair a = Pair a a

instance Foldable Pair where
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f z (Pair x y) = x `f` (y `f` z)

    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair x y) = (f x) `mappend` (f y)

data NonEmpty a = a :| [a]

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (x :| xs) = x `f` (foldr f z xs)

    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = (f x) `mappend` (foldMap f xs)

splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn x list = foldl split ([] :| []) list
  where
    split :: NonEmpty [a] -> a -> NonEmpty [a]
    split ans y
        | y == x    = ans `add` []
        | otherwise = ans `put` y -- y \= x

      where
        add :: forall b . NonEmpty b -> b -> NonEmpty b
        add (z :| xs) w = (z :| (xs ++ [w]))

        put :: forall b . NonEmpty [b] -> b -> NonEmpty [b]
        put (xs :| []) z  = ((xs ++ [z]) :| [])
        put (xs :| xss) z = (xs :| ((init xss) ++ [(last xss) ++ [z]]))

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep list = foldr1 (\y ans -> y ++ [sep] ++ ans) list

instance Semigroup (NonEmpty a) where
    (a :| as) <> (b :| bs) = a :| (as ++ b : bs)
