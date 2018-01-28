module DemoRayCasting exposing (main)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import RayCasting exposing (..)


maxRadius : Float
maxRadius =
    200


numberOfSunRays : Int
numberOfSunRays =
    50


source : RayCasting.Position
source =
    Position -200 50



-- Position 150 145


boundaries : Box
boundaries =
    { leftBottom = { x = -250, y = -250 }
    , dimensions = { width = 500, height = 500 }
    }


boxes : List Box
boxes =
    [ { leftBottom = { x = -50, y = -75 }
      , dimensions = { width = 100, height = 150 }
      }
    , { leftBottom = { x = -150, y = 75 }
      , dimensions = { width = 10, height = 15 }
      }
    ]


rays1 : List Ray
rays1 =
    rayCasting (Just ( maxRadius, numberOfSunRays )) source boundaries boxes
        |> List.map (cutRayAtRadius maxRadius)


renderBox : Box -> Form
renderBox box =
    let
        p =
            cornerToCenter box

        x =
            p.x

        y =
            p.y

        w =
            box.dimensions.width

        h =
            box.dimensions.height
    in
        rect w h |> filled blue |> move ( x, y )


main : Html msg
main =
    toHtml <|
        collage 500 500 <|
            [ circle 10 |> filled red |> move ( source.x, source.y )
            ]
                ++ List.map renderBox boxes
                -- ++ List.map (\p -> circle 10 |> filled red |> move (p.x, p.y)) (boxCorners box)
                ++ List.map (\( p1, p2 ) -> traced (solid red) (segment ( p1.x, p1.y ) ( p2.x, p2.y )))
                    (List.map pointsFromLine <| rays1)
                ++ (List.map (\x -> x |> filled red) (raysPolygons (List.sortBy rayAngle rays1)))



-- ++ case ray2coords of
--     (p1, p2) -> [traced (solid red) (segment (p1.x, p1.y) (p2.x, p2.y))]
