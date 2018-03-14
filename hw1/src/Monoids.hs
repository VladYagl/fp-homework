{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monoids
       (
         Endo(..)
       , Name(..)
       , Builder(..)
       , maybeConcat
       , eitherConcat
       , fromString
       , toString
       ) where

import qualified Data.Semigroup as Semi

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = unbox . mconcat
  where
    unbox Nothing  = []
    unbox (Just x) = x

eitherConcat :: forall a b . (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr add (mempty, mempty)
  where
    add :: Either a b -> (a, b) -> (a, b)
    add (Left a) (l, r)  = (a `mappend` l, r)
    add (Right a) (l, r) = (l, a `mappend` r)

data ThisOrThat a b = This a | That b | Both a b

instance Semi.Semigroup (ThisOrThat a b) where
    This x <> That y = Both x y
    This _ <> x = x
    That _ <> x = x
    x <> _ = x

newtype Endo a = Endo { getEndo :: a -> a  }

instance Monoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

newtype Name = Name String

instance Monoid Name where
    mempty = Name ""
    Name a `mappend` Name b = Name (a ++ "." ++ b)

data Builder = One Char | Many [Builder]

instance Monoid Builder where
    mempty = Many []

    Many as `mappend` x = Many (as ++ [x])
    x `mappend` Many as = Many ([x] ++ as)
    One x `mappend` One y = Many [One x, One y]

fromString :: String -> Builder
fromString = mconcat . map (\char -> One char)

toString :: Builder -> String
toString (One a)   = [a]
toString (Many xs) = concat (map toString xs)
