module DAG where

newtype Vertex a = Vertex (a, [a]) deriving (Eq, Show)
type Graph a = [Vertex a]

--- Parameters: Graph to insert into, value inserted, nodes connected to inserted value, nodes inserted 
--- value is connected to (these are two distinct groups, as the inserted node cannot have an edge to a preceding node)
insert :: Ord a => Graph a -> a -> [a] -> [a] -> Graph a
insert [] x _ connectedTo       = insertVertex x connectedTo [] 
insert graph x connectedFrom connectedTo
    = map (vertexInsertEdge x connectedFrom) updatedGraph where
        updatedGraph = insertVertex x connectedTo graph 

insertVertex :: Ord a => a -> [a] -> Graph a -> Graph a
insertVertex x _ []   = [Vertex (x, [])] 
insertVertex x connectedTo (Vertex (vertex, edges):vertices)
    | x < vertex    = (Vertex (x, connectedTo)) : (Vertex (vertex, edges)):vertices
    | x == vertex   = error "vertex already present"
    | x > vertex    = (Vertex (vertex, edges) : (insertVertex x connectedTo vertices))

vertexInsertEdge :: Ord a => a -> [a] -> Vertex a -> Vertex a
vertexInsertEdge x connectedVertices (Vertex (vertex, edges))
    | elem vertex connectedVertices && x < vertex = error "Cycle created by connected a vertex to a preceding vertex"
    | elem vertex connectedVertices = Vertex (vertex, x:edges)
    | otherwise     = Vertex (vertex, edges)

graphElem :: Ord a => a -> Graph a -> Bool
graphElem _ []      = False
graphElem x ((Vertex (vertex, _)):vertices)
    | x < vertex    = graphElem x vertices
    | x == vertex   = True
    | otherwise     = False

lowestCommonAncestor :: Ord a => Graph a -> Vertex a -> Vertex a -> a
lowestCommonAncestor [] _ _   = error "empty graph"
lowestCommonAncestor (vertex:vertices) (Vertex (a, aEdges)) (Vertex (b, bEdges))
    = a ---let aAncestors = filter 

distanceToAncestor :: Ord a => Graph a -> Vertex a -> Int -> Vertex a ->  Int
distanceToAncestor [] _ _ _  = error "empty graph"
distanceToAncestor _ (Vertex (b, _)) accumulator (Vertex (a, []))
    | a /= b        = 0
    | otherwise     = accumulator
distanceToAncestor graph (Vertex (b, bEdges)) accumulator (Vertex (a, aEdges))
    | a == b        = accumulator
    | elem b aEdges = accumulator+1
    | otherwise     = minimum (filter isNonZero (map (distanceToAncestor graph (Vertex (b, bEdges)) (accumulator+1)) (map (getVertex graph) aEdges)))

getVertex :: Ord a => Graph a -> a -> Vertex a
getVertex ((Vertex (vertex, edges)):vertices) x
    | vertex == x       = (Vertex (vertex, edges))
    | vertices == []    = error "Vertex not in this graph"
    | otherwise         = getVertex vertices x

isNonZero :: Int -> Bool
isNonZero x
    | x > 0             = True
    | otherwise         = False