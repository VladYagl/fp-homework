{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Lens
       ( Lens
       , Lens'
       , view
       , set
       , over
       , _1
       , _2
       , lens
       , choosing
       , (<%~)
       , (<<%~)
       ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

-- view :: Lens' s a -> s -> a
view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- set  :: Lens' s a -> b -> s -> t
set  :: Lens s t a b -> b -> s -> t
set l value = runIdentity . l (Identity . const value)

-- over :: Lens' s a -> (a -> a) -> s -> s
over :: Lens s t a b -> (a -> b) -> s -> t
over l updater = join $ set l . updater . view l

_1 :: Lens (a, x) (b, x) a b
_1 = \functor (x, y) -> (\v -> (v, y)) <$> functor x

_2 :: Lens (x, a) (x, b) a b
_2 = \functor (x, y) -> (\v -> (x, v)) <$> functor y

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens _view _set = \functor obj -> _set obj <$> functor (_view obj)

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (\case
                            Left s1 -> view l1 s1
                            Right s2 -> view l2 s2
                      )
                      (\case
                            (Left s1) -> \value -> Left $ set l1 value s1
                            (Right s2) -> \value -> Right $ set l2 value s2
                      )

-- TODO: really not sure about this one
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f $ view l s, over l f s)


(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (view l s, over l f s)
