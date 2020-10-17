module BinaryTree where

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a) deriving (Read, Eq, Show)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a lesserTree greaterTree)
    | x == a = Node x lesserTree greaterTree
    | x <  a = Node a (treeInsert x lesserTree) greaterTree
    | x >  a = Node a lesserTree (treeInsert x greaterTree)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a lesserTree greaterTree)
    | x ==  a = True
    | x <   a = treeElem x lesserTree
    | x >   a = treeElem x greaterTree

treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node a lesserTree greaterTree) = maximum [1, lh, rh]
    where   lh = 1 + treeHeight lesserTree
            rh = 1 + treeHeight greaterTree

makeTree :: (Ord a) =>  [a] -> Tree a
makeTree = foldr treeInsert EmptyTree . reverse

lca :: Ord a => a -> a -> Tree a -> a
lca m n ~(Node a lesserTree greaterTree) 
    | n < a     = lca m n lesserTree
    | m > a     = lca m n greaterTree
    | otherwise = a
