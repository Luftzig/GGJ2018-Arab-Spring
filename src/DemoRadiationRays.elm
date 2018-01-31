module DemoRadiationRays exposing (..)

import Time exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import RayCasting exposing (..)


type alias Radius =
    Float


type alias Model =
    { time : Time
    }


type Msg
    = Tick Time


initModel : Model
initModel =
    { time = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


maxRadius : Float
maxRadius =
    300


nCircles : Int
nCircles =
    10


fadingRed : Radius -> Color
fadingRed r =
    rgba 255 0 0 (1 - r / maxRadius)


mod : Float -> Float -> Float
mod x m =
    x - (toFloat <| floor (x / m)) * m


computeRadius : Radius -> Int -> Float -> Int -> Float
computeRadius maxRadius nCircles t i =
    (mod (toFloat i * maxRadius / (toFloat nCircles) + (t / 100)) maxRadius)


view : Model -> Html Msg
view model =
    toHtml <|
        collage 1000 1000 <|
            (List.range 0 nCircles
                |> List.map
                    (\i ->
                        let
                            r =
                                computeRadius maxRadius nCircles model.time i
                        in
                            (raysCircles r rays1
                             -- ++ raysCircles r (
                             --   case rays1 of
                             --     ray1::_ -> (case List.reverse rays1 of
                             --         yar1::_ -> [yar1, ray1]
                             --         _ -> [])
                             --     _ -> [])
                            )
                                |> List.map (outlined ((solid (fadingRed r)) |> \l -> { l | width = 3 }) >> move ( source.x, source.y ))
                                |> group
                    )
            )
                ++ List.map renderBox boxes



-- ++ [ traced (solid red) (segment (source.x, source.y) (fromPolar (maxRadius, (2*pi / toFloat numberOfSunRays) + pi))) ]
-- ++ [ traced (solid red) (segment (source.x, source.y) (fromPolar (maxRadius, (-2*pi / toFloat numberOfSunRays) + pi))) ]
-- ++ List.map (\p -> circle 10 |> filled red |> move (p.x, p.y)) (boxCorners box)
-- ++ List.map (\( p1, p2 ) -> traced (solid red) (segment ( p1.x, p1.y ) ( p2.x, p2.y )))
--     (List.map pointsFromLine <| rays1)
-- ++ (List.map (\x -> x |> filled red) (raysPolygons (List.sortBy rayAngle rays1)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { time = t }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


numberOfSunRays : Int
numberOfSunRays =
    100


source : RayCasting.Position
source =
    Position 50 10


boundaries : Box
boundaries =
    { leftBottom = { x = -250, y = -250 }
    , dimensions = { width = 1000, height = 1000 }
    }


boxes : List Box
boxes =
    [ { leftBottom = { x = 200, y = -75 }
      , dimensions = { width = 100, height = 150 }
      }
    , { leftBottom = { x = 100, y = 75 }
      , dimensions = { width = 10, height = 15 }
      }
    , { leftBottom = { x = -60, y = -80 }
      , dimensions = { width = 50, height = 50 }
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
