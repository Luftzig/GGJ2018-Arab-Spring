module Paths exposing (Edge, makeEdges)

import Definitions exposing (HasNode, Node, Obstacle, Position, Positioned)
import List.Extra as ListE
import RayCasting exposing (Vector, boxWalls, doesSegmentsCross, vectSub)


type alias Edge =
    { start : Position
    , end : Position
    }


makeEdges : List Obstacle -> List (Node a) -> List (Node b) -> List Edge
makeEdges obstacles nodesA nodesB =
    let
        pairs =
            nodesA
                |> ListE.andThen
                    (\x ->
                        nodesB
                            |> ListE.andThen (\y -> [ ( x, y ) ])
                    )

        isSelfPair pair =
            (.position <| Tuple.first pair) == (.position <| Tuple.second pair)

        validPairs =
            List.filter (\pair -> ((not << isSelfPair) pair) && nodesInRange pair && nodesInSight obstacles pair) pairs

        edges =
            List.map toEdge validPairs
    in
        Debug.log ("pairs " ++ toString pairs ++ "\nvalidPairs " ++ toString validPairs ++ "\nedges " ++ toString edges) edges


nodesInRange : ( Node a, Node b ) -> Bool
nodesInRange ( nodeA, nodeB ) =
    let
        dist =
            distance nodeA.position nodeB.position
    in
        dist <= nodeA.node.range && dist <= nodeB.node.range


nodesInSight : List Obstacle -> ( Node a, Node b ) -> Bool
nodesInSight obstacles ( node1, node2 ) =
    let
        allWalls =
            List.concatMap boxWalls obstacles

        vec1 =
            RayCasting.Vector node1.position.x node1.position.y

        vec2 =
            RayCasting.Vector node2.position.x node2.position.y

        nodesSegment =
            { source = vec1, direction = vectSub vec2 vec1 }
    in
        not <| List.any (doesSegmentsCross nodesSegment) allWalls


distance : Position -> Position -> Float
distance pos1 pos2 =
    sqrt <| ((pos1.x - pos2.x) ^ 2) + ((pos1.y - pos2.y) ^ 2)


toEdge : ( Node a, Node b ) -> Edge
toEdge ( node1, node2 ) =
    Edge node1.position node2.position
