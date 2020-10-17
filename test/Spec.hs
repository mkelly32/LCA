module Main where

import Test.HUnit

import BinaryTree

main :: IO ()
main = do
    runTestTT tests
    return ()

--Constants
initialValue        = 10 :: Int
lesserValue         = 5  :: Int
greaterValue        = 15 :: Int
smallestValue       = 1  :: Int
treeSizeOne         = Node initialValue EmptyTree EmptyTree
treeSizeTwo         = Node initialValue (Node lesserValue EmptyTree EmptyTree) EmptyTree
treeSizeThree       = Node initialValue (Node lesserValue EmptyTree EmptyTree) 
    (Node greaterValue EmptyTree EmptyTree)
treeSizeFour        = Node initialValue (Node lesserValue 
    (Node smallestValue EmptyTree EmptyTree) EmptyTree) (Node greaterValue EmptyTree EmptyTree)

tests = TestList [   
        TestLabel "testTreeInsertEmpty"     testTreeInsertEmpty,
        TestLabel "testTreeInsertLesser"    testTreeInsertLesser,
        TestLabel "testTreeInsertGreater"   testTreeInsertGreater,
        TestLabel "testTreeInsertSubTree"   testTreeInsertSubTree,
        TestLabel "testTreeElemEmpty"       testTreeElemEmpty,
        TestLabel "testTreeElemLeft"        testTreeElemLeft,
        TestLabel "testTreeElemRight"       testTreeElemRight,
        TestLabel "testTreeElemFalse"       testTreeElemFalse,
        TestLabel "testTreeHeightEmpty"     testTreeHeightEmpty,
        TestLabel "testTreeHeightOne"       testTreeHeightOne,
        TestLabel "testTreeHeightThree"       testTreeHeightThree,
        TestLabel "testMakeTreeOne"         testMakeTreeOne,
        TestLabel "testMakeTreeLarge"       testMakeTreeLarge
    ]

--Test treeInsert
testTreeInsertEmpty     = TestCase (treeSizeOne     @=? treeInsert initialValue EmptyTree)
testTreeInsertLesser    = TestCase (treeSizeTwo     @=? treeInsert lesserValue treeSizeOne)
testTreeInsertGreater   = TestCase (treeSizeThree   @=? treeInsert greaterValue treeSizeTwo)
testTreeInsertSubTree   = TestCase (treeSizeFour    @=? treeInsert smallestValue treeSizeThree)

--Test treeElem
testTreeElemEmpty       = TestCase (False   @=? treeElem initialValue EmptyTree)
testTreeElemLeft        = TestCase (True    @=? treeElem lesserValue treeSizeThree)
testTreeElemRight       = TestCase (True    @=? treeElem greaterValue treeSizeThree)
testTreeElemFalse       = TestCase (False   @=? treeElem smallestValue treeSizeThree)

--Test treeHeight
testTreeHeightEmpty     = TestCase (0   @=? treeHeight EmptyTree)
testTreeHeightOne       = TestCase (1   @=? treeHeight treeSizeOne)
testTreeHeightThree       = TestCase (3   @=? treeHeight treeSizeFour)

--Test makeTree
testMakeTreeOne         = TestCase (treeSizeOne     @=? makeTree [initialValue])
testMakeTreeLarge       = TestCase (treeSizeFour    @=? makeTree [initialValue, lesserValue, greaterValue, smallestValue])
