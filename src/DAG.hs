module DAG where

newtype Vertex a = Vertex (a, [a])
type Graph a = [Vertex a]

insert :: Ord a => Graph a -> a -> [a] -> Graph a
insert _ x []           = insertVertex x []
insert graph x connectedVertices
    = map (vertexInsertEdge x connectedVertices) updatedGraph where
        updatedGraph = insertVertex x graph 

insertVertex :: Ord a => a -> Graph a -> Graph a
insertVertex x []   = [Vertex (x, [])] 
insertVertex x (Vertex (vertex, edges):vertices)
    | x < vertex    = (Vertex (x, [])) : (Vertex (vertex, edges)):vertices
    | x == vertex   = error "vertex already present"
    | x > vertex    = (Vertex (vertex, edges) : (insertVertex x vertices))

vertexInsertEdge :: Ord a => a -> [a] -> Vertex a -> Vertex a
vertexInsertEdge x connectedVertices (Vertex (vertex, edges))
    | elem vertex connectedVertices = Vertex (vertex, x:edges)
    | otherwise     = Vertex (vertex, edges)