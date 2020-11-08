module Main where

import Test.HUnit

import DAG

main :: IO ()
main = do
    runTestTT tests
    return ()

--Constants
initialValue        = 10 :: Int
lesserValue         = 5  :: Int
greaterValue        = 15 :: Int
smallestValue       = 1  :: Int
smallValue          = 6  :: Int
bigValue            = 12 :: Int
largestValue        = 25 :: Int

emptyGraph          = []
graphSizeOne        = [Vertex (initialValue, [])]
graphSizeTwo        = (Vertex (initialValue, [greaterValue])): [Vertex (greaterValue, [])]
graphSizeThree      = (Vertex (lesserValue, [greaterValue])) : (Vertex (initialValue, [greaterValue])) : [(Vertex (greaterValue, []))]

tests = TestList [   
        TestLabel   "testInsertEmpty"               testInsertEmpty,
        TestLabel   "testInsertGraphSizeOne"        testInsertGraphSizeOne,
        TestLabel   "testInsertGraphSizeTwo"        testInsertGraphSizeTwo  
    ]

testInsertEmpty             = TestCase (graphSizeOne    @=? (insert emptyGraph initialValue [] []))
--- tests that the new node will be referenced by preceding nodes with an edge to it
testInsertGraphSizeOne      = TestCase (graphSizeTwo    @=? (insert graphSizeOne greaterValue [initialValue] []))
--- tests that this node will reference succeding nodes that it has an edge to
testInsertGraphSizeTwo      = TestCase (graphSizeThree  @=? (insert graphSizeTwo lesserValue [] [greaterValue]))