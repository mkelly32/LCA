module BinaryTree where

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a) deriving (Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x ==  a = True
    | x <   a = treeElem x right
    | x >   a = treeElem x left

treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node a left right) = maximum [1, lh, rh]
    where   lh = 1 + treeHeight left
            rh = 1 + treeHeight right

makeTree :: (Ord a) =>  [a] -> Tree a
makeTree = foldr treeInsert EmptyTree . reverse