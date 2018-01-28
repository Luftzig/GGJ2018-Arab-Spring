module DemoRadiation exposing (..)

import Time exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)

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

mod : Float -> Float -> Float
mod x m = x - (toFloat <| floor (x / m)) *  m

maxRadius : Float
maxRadius = 200

nCircles : Int
nCircles = 10

computeRadius : Radius -> Int -> Float -> Int -> Float
computeRadius maxRadius nCircles t i = (mod (toFloat i * maxRadius / (toFloat nCircles) + (t / 100)) maxRadius)

view : Model -> Html Msg
view model = toHtml <|
    collage 500 500 <|
        ( List.range 0 nCircles |> List.map ( \i ->
            let r = computeRadius maxRadius nCircles model.time i
            in circle r |> outlined (solid (rgba 255 0 0 (1 - r / maxRadius))) )
        )

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
