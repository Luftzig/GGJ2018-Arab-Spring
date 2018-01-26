module Main exposing (..)

import Definitions exposing (..)
import Render exposing (..)
import Html exposing (..)
import Html.Events exposing (on)
import Mouse as Mouse
import Time exposing (Time, millisecond)
import Json.Decode as Decode
import Levels exposing (level1)


initToolsState : ToolsState
initToolsState =
    [ { name = "repeater"
      , toolType = Repeater
      , nodeParameters = { range = 100 }
      , position = { x = -250, y = -250 }
      , active = False
      }
    ]


initModel : Model
initModel =
    { levels = []
    , currentLevel = level1
    , levelState = { time = 0, progress = Started, tools = initToolsState }
    , gameState = { currentLevel = -1, progress = PlayingLevel (-1) }
    , drag = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ renderModel model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Time.every millisecond Tick

        --Sub.none
        Just _ ->
            Sub.batch
                [ Mouse.moves (MouseEvent << DragAt)
                , Mouse.ups (MouseEvent << DragEnd)
                , Time.every millisecond Tick
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


mousePos2Pos : Mouse.Position -> Position
mousePos2Pos p =
    Position (toFloat p.x) (toFloat p.y)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map (MouseEvent << DragStart) Mouse.position)
