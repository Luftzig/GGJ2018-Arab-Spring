module RayCasting exposing (..)

import Collage exposing (Shape, polygon)
import Vector exposing (..)
import List.Extra exposing (minimumBy)


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
        List.map (\ray -> List.foldr cutRayBySegment ray walls) (rawRays ++ extraRays ++ starRays)
            |> case starParams of
                Nothing ->
                    identity

                -- Fit *all* the rays to the radius of the star.
                Just ( radius, nRays ) ->
                    List.map (cutRayAtRadius radius)


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
                [] ->
                    []

                [ r ] ->
                    []

                r1 :: r2 :: rs ->
                    raysPolygon r1 r2 :: raysPolygonsInternal (r2 :: rs)

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

mod : Float -> Float -> Float
mod x m = x - (toFloat <| floor (x / m)) *  m

-- compareAngles : Float -> Float -> Order
-- compareAngles phi1 phi2 =
--     if mod (phi1 + 2*pi) (2*pi) < mod (phi2 + 2*pi) (2*pi) && mod (phi2 + 2*pi) (2*pi) < mod (phi1 + 3*pi) (2*pi)
--         then LT
--         else if mod (phi2 + 2*pi) (2*pi) < mod (phi1 + 2*pi) (2*pi) && mod (phi1 + 3*pi) (2*pi) < mod (phi2 + 2*pi) (2*pi)
--             then GT
--             else EQ

compareAngles : Float -> Float -> Order
compareAngles phi1 phi2 =
    if mod (phi1 + 2*pi) (2*pi) < mod (phi2 + 2*pi) (2*pi)
        then LT
        else if mod (phi1 + 2*pi) (2*pi) > mod (phi2 + 2*pi) (2*pi)
            then GT
            else EQ

raysMidRadius : List Ray -> Float -> Float
raysMidRadius rays angle =
    let nearestRay1 = minimumBy (\ray -> rayAngle ray - angle) <| List.filter (\ray -> compareAngles (rayAngle ray) angle == GT ) rays
        nearestRay2 = minimumBy (\ray -> angle - rayAngle ray) <| List.filter (\ray -> compareAngles (rayAngle ray) angle == LT ) rays
    in Maybe.withDefault 0 <| Maybe.map2 (\r1 r2 -> max (rayRadius r1) (rayRadius r2)) nearestRay1 nearestRay2

circ : Float -> Float -> (Float, Float)
circ r phi = (r * cos phi, r * sin phi)

-- circle : Float -> Shape
-- circle r =
--     let n = 50
--         t = 2 * pi / n
--         f i = circ r (t * i)
--     in polygon <| List.map (f << toFloat) <| List.range 0 (n-1)

raysCircle : List Ray -> Float -> Shape
raysCircle rays r =
    let n = 50
        t = 2 * pi / n
        angle i = t * i
        radius i = min (raysMidRadius rays (angle i)) r
        f i = circ (radius i) (angle i)
    in polygon <| List.map (f << toFloat) <| List.range 0 (n-1)


raysPairCircle : Ray -> Ray -> Float -> Shape
raysPairCircle ray1 ray2 r =
    let n = 50
        mn = min (rayAngle ray1) (rayAngle ray2)
        mx = max (rayAngle ray1) (rayAngle ray2)
        angleDiff = mx - mn -- mod (mx - mn + 2*pi) (2*pi)
        t = angleDiff / n
        angle i = mn + t * i
        radius i = min (max (rayRadius ray1) (rayRadius ray2)) r
        f i = circ (radius i) (angle i)
    in polygon <| List.map (f << toFloat) <| List.range 0 (n-1)

raysCircles : List Ray -> Float -> List Shape
raysCircles rays0 r = case rays0 of
    []  ->  []
    [_]  ->  []
    ray1::ray2::rays  ->  raysPairCircle ray1 ray2 r :: raysCircles (ray2::rays) r
