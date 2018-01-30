module RayCasting exposing (..)

import Collage exposing (Shape, polygon)
import Vector exposing (..)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type alias Box =
    { leftBottom : Position
    , dimensions : Dimensions
    }


type alias Line =
    { source : Vector
    , direction : Vector
    }


type alias Ray =
    Line


type alias Segment =
    Line


cornerToCenter : Box -> Position
cornerToCenter b =
    { x = b.leftBottom.x + b.dimensions.width / 2
    , y = b.leftBottom.y + b.dimensions.height / 2
    }


boxCorners : Box -> List Position
boxCorners box =
    let
        { x, y } =
            box.leftBottom

        ( w, h ) =
            ( box.dimensions.width
            , box.dimensions.height
            )
    in
        [ Position x y
        , Position x (y + h)
        , Position (x + w) y
        , Position (x + w) (y + h)
        ]


boxWalls : Box -> List Segment
boxWalls box =
    let
        ( x, y, w, h ) =
            ( box.leftBottom.x
            , box.leftBottom.y
            , box.dimensions.width
            , box.dimensions.height
            )
    in
        [ { source = Vector x y
          , direction = Vector w 0
          }
        , { source = Vector x (y + h)
          , direction = Vector 0 (-h)
          }
        , { source = Vector (x + w) y
          , direction = Vector 0 h
          }
        , { source = Vector (x + w) (y + h)
          , direction = Vector (-w) 0
          }
        ]


raysToBoxCorners : Position -> Box -> List Ray
raysToBoxCorners source box =
    let
        ( walls, corners ) =
            ( boxWalls box
            , boxCorners box
            )

        rawRays =
            List.map (\dst -> lineFromPoints source dst) corners
    in
        List.map (\ray -> List.foldr cutRayBySegment ray walls) rawRays


createStar : Float -> Int -> Position -> List Ray
createStar radius nRays source =
    let
        angles =
            List.map (\i -> (2 * pi) * (toFloat i / toFloat nRays)) (List.range 0 nRays)

        rays =
            List.map (\phi -> fromPolar ( radius, phi )) angles

        src =
            Vector source.x source.y
    in
        List.map (\( x, y ) -> { source = src, direction = Vector x y }) rays


rayCasting : Maybe ( Float, Int ) -> Position -> Box -> List Box -> List Ray
rayCasting starParams source boundaries obstacles =
    let
        -- Triangle inequality
        maxScale =
            boundaries.dimensions.width + boundaries.dimensions.height

        walls =
            List.concat <| List.map boxWalls (boundaries :: obstacles)

        corners =
            List.concat <| List.map boxCorners (boundaries :: obstacles)

        rawRays =
            List.map (\dst -> lineFromPoints source dst) corners

        -- Magic number! maybe it should be lifted?
        extraAngle =
            0.00001

        rotateAndScale phi ray =
            { ray | direction = vectScale maxScale <| vectRotate phi ray.direction }

        extraRays =
            List.map (rotateAndScale extraAngle) rawRays
                ++ List.map (rotateAndScale (-extraAngle)) rawRays

        starRays =
            case starParams of
                Nothing ->
                    []

                Just ( radius, nRays ) ->
                    createStar radius nRays source
    in
        List.sortBy rayAngle
            (List.map (\ray -> List.foldr cutRayBySegment ray walls) (rawRays ++ extraRays ++ starRays)
                |> case starParams of
                    Nothing ->
                        identity

                    -- Fit *all* the rays to the radius of the star.
                    Just ( radius, nRays ) ->
                        List.map (cutRayAtRadius radius)
            )


lineFromPoints : Position -> Position -> Line
lineFromPoints p1 p2 =
    { source = Vector (p1.x) (p1.y)
    , direction = vectSub (Vector (p2.x) (p2.y)) (Vector (p1.x) (p1.y))
    }


pointsFromLine : Line -> ( Position, Position )
pointsFromLine l =
    ( l.source
    , vectAdd l.source l.direction
    )


computeCrossParams : Line -> Line -> Maybe ( Float, Float )
computeCrossParams l1 l2 =
    let
        ( x1, y1 ) =
            vect2pair l1.source

        ( dx1, dy1 ) =
            vect2pair l1.direction

        ( x2, y2 ) =
            vect2pair l2.source

        ( dx2, dy2 ) =
            vect2pair l2.direction
    in
        if dx1 * dy2 == dx2 * dy1 then
            Nothing
        else
            let
                det =
                    dx1 * dy2 - dx2 * dy1

                ( t1, t2 ) =
                    ( (dx2 * (y1 - y2) - dy2 * (x1 - x2)) / det
                    , (dx1 * (y2 - y1) - dy1 * (x2 - x1)) / -det
                    )
            in
                Just ( t1, t2 )


cutRayBySegment : Segment -> Ray -> Ray
cutRayBySegment s r =
    case computeCrossParams s r of
        Nothing ->
            r

        Just ( _, t2 ) ->
            if doesSegmentsCross s r then
                { r | direction = vectScale t2 r.direction }
            else
                r


doesSegmentsCross : Segment -> Ray -> Bool
doesSegmentsCross s r =
    case computeCrossParams s r of
        Nothing ->
            False

        Just ( t1, t2 ) ->
            (t1 > 0) && (t1 < 1) && (t2 > 0) && (t2 < 1)


rayAngle : Ray -> Float
rayAngle ray =
    (\( _, phi ) -> phi) <|
        toPolar
            (vect2pair ray.direction)


rayRadius : Ray -> Float
rayRadius ray =
    (\( r, _ ) -> r) <|
        toPolar
            (vect2pair ray.direction)


rotateRay : Float -> Ray -> Ray
rotateRay angle ray =
    let
        ( r, phi ) =
            toPolar (vect2pair ray.direction)
    in
        { ray | direction = pair2vect <| fromPolar ( r, phi + angle ) }


cutRayAtRadius : Float -> Ray -> Ray
cutRayAtRadius radius ray =
    let
        d =
            ray.direction

        ( r, phi ) =
            toPolar <| vect2pair d

        ( x, y ) =
            fromPolar ( min radius r, phi )
    in
        { ray | direction = Vector x y }


raysPolygon : Ray -> Ray -> Shape
raysPolygon r1 r2 =
    let
        ( s, p1 ) =
            pointsFromLine r1

        ( _, p2 ) =
            pointsFromLine r2
    in
        polygon [ ( s.x, s.y ), ( p1.x, p1.y ), ( p2.x, p2.y ) ]


raysPolygons : List Ray -> List Shape
raysPolygons rays =
    let
        raysPolygonsInternal rays =
            case rays of
                r1 :: r2 :: rs ->
                    raysPolygon r1 r2 :: raysPolygonsInternal (r2 :: rs)

                _ ->
                    []

        ( hd, lt ) =
            ( List.head rays
            , List.head (List.reverse rays)
            )

        otherPolygons =
            raysPolygonsInternal rays
    in
        case Maybe.map2 raysPolygon hd lt of
            Nothing ->
                otherPolygons

            Just p ->
                p :: otherPolygons


circ : Float -> Float -> ( Float, Float )
circ r phi =
    ( r * cos phi, r * sin phi )



-- circle : Float -> Shape
-- circle r =
--     let n = 50
--         t = 2 * pi / n
--         f i = circ r (t * i)
--     in polygon <| List.map (f << toFloat) <| List.range 0 (n-1)


raysPairCircle : Float -> Ray -> Ray -> Shape
raysPairCircle r ray1 ray2 =
    let
        n =
            10

        mn =
            rayAngle ray1

        -- min (rayAngle ray1) (rayAngle ray2)
        mx =
            rayAngle ray2

        -- max (rayAngle ray1) (rayAngle ray2)
        dot ( x1, y1 ) ( x2, y2 ) =
            x1 * x2 + y1 * y2

        angleDiff =
            acos (dot (fromPolar ( 1, mn )) (fromPolar ( 1, mx )))

        t =
            angleDiff / n

        angle i =
            mn + t * i

        radius i =
            min (max (rayRadius ray1) (rayRadius ray2)) r

        f i =
            circ (radius i) (angle i)
    in
        polygon <| List.map (f << toFloat) <| List.range 0 (n - 1)


raysCircles : Float -> List Ray -> List Shape
raysCircles r rays0 =
    let
        raysCirclesInternal rays =
            case rays of
                ray1 :: ray2 :: moreRays ->
                    raysPairCircle r ray1 ray2 :: raysCirclesInternal (ray2 :: moreRays)

                _ ->
                    []
    in
        raysCirclesInternal rays0
            ++ case ( rays0, List.reverse rays0 ) of
                ( ray1 :: _, yar1 :: _ ) ->
                    [ raysPairCircle r yar1 ray1 ]

                _ ->
                    []
