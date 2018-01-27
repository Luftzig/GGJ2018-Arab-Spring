module RayCasting exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)


-- import List exposing (..)
-- import Debug


type alias Position =
    { x : Float
    , y : Float
    }


type Vector
    = Vector Float Float


vX : Vector -> Float
vX (Vector x _) =
    x


vY : Vector -> Float
vY (Vector _ y) =
    y


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


boxCorners : Box -> List Position
boxCorners box =
    let
        x =
            box.leftBottom.x

        y =
            box.leftBottom.y

        w =
            box.dimensions.width

        h =
            box.dimensions.height
    in
        [ Position x y
        , Position x (y + h)
        , Position (x + w) y
        , Position (x + w) (y + h)
        ]


boxWalls : Box -> List Segment
boxWalls box =
    let
        x =
            box.leftBottom.x

        y =
            box.leftBottom.y

        w =
            box.dimensions.width

        h =
            box.dimensions.height
    in
        [ { source = Vector x y, direction = Vector w 0 }
        , { source = Vector x (y + h), direction = Vector 0 (-h) }
        , { source = Vector (x + w) y, direction = Vector 0 h }
        , { source = Vector (x + w) (y + h), direction = Vector (-w) 0 }
        ]


raysToBoxCorners : Position -> Box -> List Ray
raysToBoxCorners source box =
    let
        walls =
            boxWalls box

        corners =
            boxCorners box

        rawRays =
            List.map (\dst -> lineFromPoints source dst) corners
    in
        List.map (\ray -> List.foldr cutRay ray walls) rawRays




rayTracing : Position -> Box -> List Box -> List Ray
rayTracing source boundaries obstacles =
    let
        maxScale =
            boundaries.dimensions.width ^ 2 + boundaries.dimensions.height ^ 2

        walls =
            List.concat <| List.map boxWalls (boundaries::obstacles)

        corners =
            List.concat <| List.map boxCorners (boundaries::obstacles)

        rawRays =
            List.map (\dst -> lineFromPoints source dst) corners

        dphi = 0.00001

        extraRays =
               List.map (\ray -> {ray | direction = vectScale maxScale <| vectRotate dphi ray.direction}) rawRays
            ++ List.map (\ray -> {ray | direction = vectScale maxScale <| vectRotate (-dphi) ray.direction}) rawRays
    in
        List.map (\ray -> List.foldr cutRay ray walls) (rawRays ++ extraRays)


vectAdd : Vector -> Vector -> Vector
vectAdd (Vector x1 y1) (Vector x2 y2) =
    (Vector (x1 + x2) (y1 + y2))


vectSub : Vector -> Vector -> Vector
vectSub (Vector x1 y1) (Vector x2 y2) =
    (Vector (x1 - x2) (y1 - y2))


vectScale : Float -> Vector -> Vector
vectScale s (Vector x y) =
    Vector (s * x) (s * y)

vectRotate : Float -> Vector -> Vector
vectRotate phi (Vector x0 y0) =
    let (r,p) = toPolar (x0, y0)
        (x1,y1) = fromPolar (r,p + phi)
    in Vector x1 y1

lineFromPoints : Position -> Position -> Line
lineFromPoints p1 p2 =
    { source = Vector (p1.x) (p1.y)
    , direction = vectSub (Vector (p2.x) (p2.y)) (Vector (p1.x) (p1.y))
    }


pointsFromLine : Line -> ( Position, Position )
pointsFromLine l =
    ( { x = vX l.source, y = vY l.source }
    , let
        p =
            vectAdd l.source l.direction
      in
        { x = vX p, y = vY p }
    )


cutRay : Segment -> Ray -> Ray
cutRay s r =
    let
        x1 =
            vX r.source

        y1 =
            vY r.source

        dx1 =
            vX r.direction

        dy1 =
            vY r.direction

        x2 =
            vX s.source

        y2 =
            vY s.source

        dx2 =
            vX s.direction

        dy2 =
            vY s.direction
    in
        if (dx1 * dy2 == dx2 * dy1) && (dx2 /= 0) then
            r
        else
            let
                det =
                    dx1 * dy2 - dx2 * dy1

                t1 =
                    (dx2 * (y1 - y2) - dy2 * (x1 - x2)) / det

                t2 =
                    (dx1 * (y2 - y1) - dy1 * (x2 - x1)) / -det
            in
                if (t1 > 0) && (t1 < 1) && (t2 > 0) && (t2 < 1) then
                    { r | direction = vectScale t1 r.direction }
                else
                    r

raysPolygon : Ray -> Ray -> Shape
raysPolygon r1 r2 =
    let
        ( s, p1 ) =
            pointsFromLine r1

        ( _, p2 ) =
            pointsFromLine r2

        sx =
            s.x

        sy =
            s.y

        rx1 =
            p1.x

        ry1 =
            p1.y

        rx2 =
            p2.x

        ry2 =
            p2.y
    in
        polygon [ ( sx, sy ), ( rx1, ry1 ), ( rx2, ry2 ) ]


raysPolygonsInternal : List Ray -> List Shape
raysPolygonsInternal rays =
    case rays of
        [] ->
            []

        [ r ] ->
            []

        r1 :: r2 :: rs -> raysPolygon r1 r2 :: raysPolygonsInternal (r2 :: rs)


raysPolygons : List Ray -> List Shape
raysPolygons rays =
    let hd = List.head rays
        lt = List.head (List.reverse rays)
        otherPolygons = raysPolygonsInternal rays
    in case Maybe.map2 raysPolygon hd lt of
        Nothing -> otherPolygons
        Just p  -> p :: otherPolygons


rayAngle : Ray -> Float
rayAngle ray =
    (\( _, phi ) -> phi) <|
        toPolar
            (let
                d =
                    ray.direction
             in
                ( vX d, vY d )
            )





-----------------------------------------------------------------------------------
---- DEMO -------------------------------------------------------------------------
-----------------------------------------------------------------------------------

source : Position
source =
    Position 150 145

boundaries : Box
boundaries =
    { leftBottom = { x = -250, y = -250 }
    , dimensions = { width = 500, height = 500 }
    }

box : Box
box =
    { leftBottom = { x = -50, y = -75 }
    , dimensions = { width = 100, height = 150 }
    }


rays1 : List Ray
rays1 =
    rayTracing source boundaries [box]


main : Html msg
main =
    toHtml <|
        collage 500 500 <|
            [ rect 100 150 |> filled blue
            , circle 10 |> filled red |> move ( source.x, source.y )
            ]
                -- ++ List.map (\p -> circle 10 |> filled red |> move (p.x, p.y)) (boxCorners box)
                ++ List.map (\( p1, p2 ) -> traced (solid red) (segment ( p1.x, p1.y ) ( p2.x, p2.y )))
                    (List.map pointsFromLine <| rays1)
                ++ (List.map (\x -> x |> filled red) (raysPolygons (List.sortBy rayAngle rays1)))



-- ++ case ray2coords of
--     (p1, p2) -> [traced (solid red) (segment (p1.x, p1.y) (p2.x, p2.y))]
