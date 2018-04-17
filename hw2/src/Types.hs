module Types
       ( stringSum
       , Optional(..)
       , NonEmpty(..)
       ) where

import Text.Read

stringSum :: String -> Maybe Int
stringSum string = traverse readMaybe (words string) >>= Just . sum

newtype Optional a = Optional (Maybe (Maybe a))

instance Functor Optional where
    -- fmap :: (a -> b) -> f a -> f b
    fmap _ (Optional Nothing)         = Optional Nothing
    fmap _ (Optional (Just Nothing))  = Optional (Just Nothing)
    fmap f (Optional (Just (Just a))) = pure (f a)

instance Applicative Optional where
    -- pure :: a -> f a
    pure = Optional . Just . Just

    -- (<*>) :: f (a -> b) -> f a -> f b
    Optional Nothing <*> _         = Optional Nothing
    Optional (Just Nothing) <*> _  = Optional (Just Nothing)
    Optional (Just (Just f)) <*> m = f <$> m

instance Monad Optional where
    -- (>>=) :: forall a b. Optional a -> (a -> Optional b) -> Optional b
    Optional Nothing >>= _         = Optional Nothing
    Optional (Just Nothing) >>= _  = Optional (Just Nothing)
    Optional (Just (Just a)) >>= f = f a

instance Foldable Optional where
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr _ z (Optional Nothing)         = z
    foldr _ z (Optional (Just Nothing))  = z
    foldr f z (Optional (Just (Just a))) = f a z

instance Traversable Optional where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse _ (Optional Nothing)         = pure (Optional Nothing)
    traverse _ (Optional (Just Nothing))  = pure (Optional (Just Nothing))
    traverse f (Optional (Just (Just a))) = pure <$> f a

-- -- NonEmpty -- --

data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Foldable NonEmpty where
    -- foldr :: (a -> b -> b) -> b -> f a -> b
    foldr f z (x :| xs) = x `f` foldr f z xs

    -- foldMap :: Monoid m => (a -> m) -> f a -> m
    foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Functor NonEmpty where
    fmap f (a :| xs) = f a :| fmap f xs

instance Applicative NonEmpty where
    pure a = a :| []

    fAll@(f :| fs) <*> (x :| xs) = f x :| (concatMap (<$> xs) fAll ++ map ($ x) fs)

instance Monad NonEmpty where
    x :| xs >>= f = foldl merge (f x) (map f xs)
      where
        merge (a :| as) (b :| bs) = a :| (as ++ b : bs)

instance Traversable NonEmpty where
    -- traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse f (x :| xs) = foldr (fAdd . f) (fmap (\y -> y :| []) (f x)) xs
      where
        add b (z :| zs) = z :| (b : zs)

        -- fAdd :: f b -> f (NonEmpty b) -> f (NonEmpty b)
        fAdd fb fnonB = fmap add fb <*> fnonB


-- Optional
-- 1. Left identity
-- return a >>= f === Optional (Just (Just a)) >>= f
--                === f a
-- 2. Right indentity
-- Optional Nothing         >>= return === Optional Nothing
-- Optional (Just Nothing)  >>= return === Optional (Just Nothing)
-- Optional (Just (Just a)) >>= return === Optianal (Just (Just a))
--
-- 3. Associativity
-- (Optional Nothing >>= f) >>= g === Optinal Nothing >>= g === Optional Nothing
-- Optional Nothing >>= (\x f x >>= g) === Optional Nothing
--
-- (Optional (Just Nothing) >>= f) >>= g === Optinal (Just Nothing) >>= g === Optional (Just Nothing)
-- Optional (Just Nothing) >>= (\x f x >>= g) === Optional (Just Nothing)
--
-- (Optional (Just (Just a)) >>= f) >>= g         === f a >>= g
-- Optional (Just (Just a)) >>= (\x -> f x >>= g) === f a >>= g
