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
graphSizeOne        = [Vertex (initialValue, [], [])]
graphSizeTwo        = (Vertex (initialValue, [], [greaterValue])): [Vertex (greaterValue, [initialValue], [])]
graphSizeThree      = (Vertex (lesserValue, [], [greaterValue])) : (Vertex (initialValue, [], [greaterValue])) : [(Vertex (greaterValue, [lesserValue, initialValue], []))]
graphSizeSix       = (Vertex (smallestValue, [], [initialValue])) : (Vertex (smallValue, [], [initialValue])) : (Vertex (initialValue, [smallestValue, smallValue], [bigValue])) : (Vertex (bigValue, [initialValue], [greaterValue, largestValue])) : (Vertex (greaterValue, [bigValue], [largestValue])) : [(Vertex (largestValue, [bigValue, greaterValue], []))]

tests = TestList [   
        TestLabel   "testInsertEmpty"               testInsertEmpty,
        TestLabel   "testInsertGraphSizeOne"        testInsertGraphSizeOne,
        TestLabel   "testInsertGraphSizeTwo"        testInsertGraphSizeTwo,
        TestLabel   "testGetAncestorsA"             testGetAncestorsA,
        TestLabel   "testGetAncestorsB"             testGetAncestorsB,
        TestLabel   "testGetAncestorsC"             testGetAncestorsC,
        TestLabel   "testLCA1"                      testLCA1,
        TestLabel   "testLCA2"                      testLCA2
    ]

testInsertEmpty             = TestCase (graphSizeOne    @=? (insert emptyGraph initialValue [] []))
--- tests that the new node will be referenced by preceding nodes with an edge to it
testInsertGraphSizeOne      = TestCase (graphSizeTwo  @=? (insert graphSizeOne greaterValue [initialValue] []))
--- tests that this node will reference succeding nodes that it has an edge to
testInsertGraphSizeTwo      = TestCase (graphSizeThree  @=? (insert graphSizeTwo lesserValue [] [greaterValue]))


testGetAncestorsA           = TestCase ([(smallestValue, 1), (smallValue, 1)] @=? getAncestors graphSizeSix 1 [(Vertex (initialValue, [smallestValue, smallValue], [bigValue]))])
testGetAncestorsB           = TestCase ([(initialValue, 1), (smallestValue, 2), (smallValue, 2)] @=? getAncestors graphSizeSix 1 [(Vertex (bigValue, [initialValue], [greaterValue, largestValue]))])
testGetAncestorsC           = TestCase ([(bigValue, 1), (initialValue, 2), (smallestValue, 3), (smallValue, 3)] @=? getAncestors graphSizeSix 1 [(Vertex (greaterValue, [bigValue], [largestValue]))])

testLCA1                    = TestCase (bigValue @=? lowestCommonAncestor graphSizeSix (Vertex (greaterValue, [bigValue], [largestValue])) (Vertex (largestValue, [bigValue, greaterValue], [])))
testLCA2                    = TestCase (initialValue @=? lowestCommonAncestor graphSizeSix (Vertex (bigValue, [initialValue], [greaterValue, largestValue])) (Vertex (greaterValue, [bigValue], [largestValue])))