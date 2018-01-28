module DemoRadiationRays exposing (..)

import Time exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)

import RayCasting exposing (..)

type alias Radius = Float
-- type alias Wavenumber = Int
--
-- type alias Radiation = Wavenumber -> Time -> Radius

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
maxRadius = 300

nCircles : Int
nCircles = 10

computeRadius : Radius -> Int -> Float -> Int -> Float
computeRadius maxRadius nCircles t i = (mod (toFloat i * maxRadius / (toFloat nCircles) + (t / 100)) maxRadius)

view : Model -> Html Msg
view model = toHtml <|
    collage 500 500 <|
        ( List.range 0 nCircles |> List.map ( \i ->
            let r = computeRadius maxRadius nCircles model.time i
            in raysCircle (List.sortBy rayAngle rays1) r |> outlined (solid (rgba 255 0 0 (1 - r / maxRadius))) |> move ( source.x, source.y ) )
        )
            ++ List.map renderBox boxes
            -- ++ List.map (\p -> circle 10 |> filled red |> move (p.x, p.y)) (boxCorners box)
            -- ++ List.map (\( p1, p2 ) -> traced (solid red) (segment ( p1.x, p1.y ) ( p2.x, p2.y )))
            --     (List.map pointsFromLine <| rays1)
            -- ++ (List.map (\x -> x |> filled red) (raysPolygons (List.sortBy rayAngle rays1)))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    Tick t -> ( { time = t }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Time.every millisecond Tick

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
    Position -200 10


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
        rect w h |> outlined (solid blue) |> move ( x, y )
