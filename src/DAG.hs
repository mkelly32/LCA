module DAG where

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
lowestCommonAncestor graph (Vertex (a, toA, fromA)) (Vertex (b, toB, fromB)) = a
   --- (map (distanceToAncestor graph (Vertex (a, aEdges)) 1) graph) (map (distanceToAncestor graph (Vertex (b, bEdges)) 1) graph)

getVertex :: Ord a => Graph a -> a -> Vertex a
getVertex ((Vertex (vertex, edgesFrom, edgesTo)):vertices) x
    | vertex == x       = (Vertex (vertex, edgesFrom, edgesTo))
    | vertices == []    = error "Vertex not in this graph"
    | otherwise         = getVertex vertices x

isNonZero :: Int -> Bool
isNonZero x
    | x > 0             = True
    | otherwise         = False