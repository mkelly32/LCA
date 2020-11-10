module DAG where
import Data.List

newtype Vertex a = Vertex (a, [a], [a]) deriving (Eq, Show)
type Graph a = [Vertex a]

--- Parameters: Graph to insert into, value inserted, nodes connected to inserted value, nodes inserted 
--- value is connected to (these are two distinct groups, as the inserted node cannot have an edge to a preceding node)
insert :: Ord a => Graph a -> a -> [a] -> [a] -> Graph a
insert [] x _ connectedTo       = insertVertex [] x [] connectedTo  
insert graph x connectedFrom connectedTo
    = map (vertexInsertEdge x connectedFrom connectedTo) updatedGraph where
        updatedGraph = insertVertex graph x connectedFrom connectedTo 

insertVertex :: Ord a => Graph a -> a -> [a] -> [a] -> Graph a
insertVertex [] x connectedFrom connectedTo   = [Vertex (x, connectedFrom, connectedTo)] 
insertVertex (Vertex (vertex, edgesTo, edgesFrom):vertices) x connectedFrom connectedTo 
    | x < vertex    = (Vertex (x, connectedFrom, connectedTo)) : (Vertex (vertex, edgesTo, edgesFrom)):vertices
    | x == vertex   = error "vertex already present"
    | x > vertex    = (Vertex (vertex, edgesTo, edgesFrom) : (insertVertex vertices x connectedFrom connectedTo))

vertexInsertEdge :: Ord a => a -> [a] -> [a] -> Vertex a -> Vertex a
vertexInsertEdge x connectedFrom connectedTo (Vertex (vertex, edgesTo, edgesFrom))
    | elem vertex connectedFrom && x < vertex   = error "Cycle created by conneccting a vertex to a preceding vertex"
    | elem vertex connectedFrom                 = Vertex (vertex, edgesTo, x:edgesFrom)
    | elem vertex connectedTo                   = Vertex (vertex, x:edgesTo, edgesFrom)
    | otherwise                                 = Vertex (vertex, edgesTo, edgesFrom)

graphElem :: Ord a => a -> Graph a -> Bool
graphElem _ []      = False
graphElem x ((Vertex (vertex, _, _)):vertices)
    | x < vertex    = graphElem x vertices
    | x == vertex   = True
    | otherwise     = False

lowestCommonAncestor :: Ord a => Graph a -> Vertex a -> Vertex a -> a
lowestCommonAncestor [] _ _ = error "empty graph"
lowestCommonAncestor graph a b = let
    aAncestors = getAncestors graph 1 [a]
    bAncestors = getAncestors graph 1 [b] in
    lowestCostSharedAncestor (sortBy sortTuples (keepCommonAncestors aAncestors bAncestors)) (sortBy sortTuples (keepCommonAncestors bAncestors aAncestors)) (head (keepCommonAncestors aAncestors bAncestors))


---Takes a single vertex in a list as its initial parameter (its wrapped in a list to aid with recursion) and returns a list of tuples, in which each tuple has a value of a vertex, and its distance from the inital vertex
getAncestors :: Ord a => Graph a -> Int -> [Vertex a] -> [(a, Int)]
getAncestors _ _ []             = []
getAncestors graph accumulator ((Vertex (_, connectedFrom, _)):vertices)
    | connectedFrom == []       = makePairs connectedFrom accumulator
    | otherwise                 = removeDuplicates ((makePairs connectedFrom accumulator) ++ getAncestors graph (accumulator+1) (map (getVertex graph) connectedFrom) ++ getAncestors graph accumulator vertices)

getVertex :: Ord a => Graph a -> a -> Vertex a
getVertex ((Vertex (vertex, edgesFrom, edgesTo)):vertices) x
    | vertex == x       = (Vertex (vertex, edgesFrom, edgesTo))
    | vertices == []    = error "Vertex not in this graph"
    | otherwise         = getVertex vertices x

isNonZero :: Int -> Bool
isNonZero x
    | x > 0             = True
    | otherwise         = False

removeDuplicates :: Ord a => [(a, Int)] -> [(a, Int)]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | elem x xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

makePairs :: Ord a => [a] -> Int -> [(a, Int)]
makePairs [] _          = []
makePairs (x:xs) cost   = (x, cost) : (makePairs xs cost)

keepCommonAncestors :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
keepCommonAncestors [] _            = []
keepCommonAncestors ((aValue, aCost):as) b
    | lookup aValue b == Nothing    = keepCommonAncestors as b
    | otherwise                     = (aValue, aCost) : (keepCommonAncestors as b)

---This function assumes it is given two lists of the same length, sorted with the values in order. Each list has a differnt cost  for the same value though. Returns the value with the lowest cost (of the maximum of each value)
lowestCostSharedAncestor :: Ord a => [(a, Int)] -> [(a, Int)] -> (a, Int) -> a
lowestCostSharedAncestor [] _ (candidateValue, _)     = candidateValue
lowestCostSharedAncestor ((aValue, aCost):as) ((_, bCost):bs) (candidateValue, candidateCost)
    | max aCost bCost < candidateCost       = lowestCostSharedAncestor as bs (aValue, (max aCost bCost))
    | otherwise                             = lowestCostSharedAncestor as bs (candidateValue, candidateCost)

sortTuples :: Ord a => (a, Int) -> (a, Int) -> Ordering
sortTuples (aValue, _) (bValue, _)
    | aValue < bValue       = LT
    | aValue == bValue      = EQ
    | otherwise             = GT

