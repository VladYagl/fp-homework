{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RecordWildCards #-}

module Tree (
         Tree(..)
       , empty
       , size
       , find
       , insert
       , delete
       , fromList
       ) where

data Tree a = Leaf | Node
    { info        :: [a]
    , left, right :: Tree a
    }


empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

size :: Tree a -> Int
size Leaf                      = 0
size Node{left = l, right = r} = 1 + size l + size r

find :: Ord a => a -> Tree a -> Maybe (Tree a)
find _ Leaf     = Nothing
find x this@Node{..}
    | x < head info = find x left
    | x > head info = find x right
    | otherwise = Just this -- x == head

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf     = Node{info = [x], left = Leaf, right = Leaf}
insert x this@Node{..}
    | x < head info = this{left = insert x left}
    | x > head info = this{right = insert x right}
    | otherwise = this{info = x : info} -- x == head

merge :: Tree a -> Tree a -> Tree a
merge Leaf second = second
merge first Leaf = first
merge first@Node{right = firstR} second@Node{left = leftL} = first{right = second{left = merge firstR leftL}}

delete :: Ord a => a -> Tree a -> Tree a
delete _ this@Leaf     = this
delete x this@Node{..}
    | x < head info = this{left = delete x left}
    | x > head info = this{right = delete x right}
    | x == head info && length info > 1 = this{info = tail info}
    | otherwise = merge left right -- x == head info && length info == 1

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ z Leaf     = z
    foldr f z Node{..} = foldr f (foldr f (foldr f z right) info) left

    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf     = mempty
    foldMap f Node{..} = foldMap f left `mappend` foldMap f info `mappend` foldMap f right
