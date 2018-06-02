{-# LANGUAGE Rank2Types #-}

module Lens where

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

data Unit = Unit
    { _hp     :: Int
    , _damage :: Int
    }

hp :: Lens' Unit Int
hp = lens _hp (\_unit h -> Unit{ _hp = h })

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
