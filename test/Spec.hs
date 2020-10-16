module Main where

import Test.HUnit

import BinaryTree

main :: IO ()
main = return ()

tests = TestList [   
        TestLabel "testTreeInsertEmpty" testTreeInsertEmpty,
        TestLabel "testTreeInsertLesser" testTreeInsertLesser,
        TestLabel "testTreeHeightEmpty" testTreeHeightEmpty
    ]

testTreeInsertEmpty     = TestCase ((treeInsert (10 :: Int) EmptyTree) @=? (Node (10 :: Int) EmptyTree EmptyTree))
testTreeInsertLesser    = TestCase (
    let treeSizeOne = treeInsert (10 :: Int) EmptyTree in
    assertFailure "failed")--Node (5 :: Int) EmptyTree treeSizeOne @=?treeInsert (5 :: Int) treeSizeOne)
testTreeHeightEmpty     = TestCase (0 @=? (treeHeight EmptyTree))