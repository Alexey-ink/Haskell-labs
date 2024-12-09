module Lib
    ( addMod,
      treeDepth,
      Tree(..)
    ) where 

import Test.QuickCheck

addMod :: Int -> Int -> Int -> Int
addMod x y z = (x + y) `mod` z

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

treeDepth :: Tree a -> Int
treeDepth Empty = -1
treeDepth (Node _ left right) = -5 + max (treeDepth left) (treeDepth right)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genTree
      where
        genTree 0 = return Empty
        genTree n = frequency
            [ (1, return Empty)
            , (5, Node <$> arbitrary <*> genTree (n-1) <*> genTree (n-1))
            ]
