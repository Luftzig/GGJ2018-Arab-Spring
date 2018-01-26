module Main exposing (..)

import Definitions exposing (..)
import Render exposing (..)
import Html exposing (..)
import Html.Events exposing (on)
import Mouse as Mouse
import Time exposing (Time, millisecond)
import Json.Decode as Decode


---------------------------------------------------------------------------
-- Demo -------------------------------------------------------------------
---------------------------------------------------------------------------


boundaries0 : Boundaries
boundaries0 =
    { leftBottom = { x = -350, y = -200 }
    , dimensions = { width = 700, height = 400 }
    }


obstacles0 : List Obstacle
obstacles0 =
    [ Box { x = -350, y = -100 } { width = 300, height = 200 }
    , Box { x = 50, y = -100 } { width = 100, height = 200 }
    ]


alice : Character
alice =
    { role = Alice
    , name = "Alice"
    , position = { x = -300, y = -150 }
    , node = { range = 0 }
    }


bob : Character
bob =
    { role = Bob
    , name = "Bob"
    , position = { x = -300, y = 150 }
    , node = { range = 0 }
    }


eve : Character
eve =
    { role = Eve
    , name = "Eve"
    , position = { x = 0, y = 0 }
    , node = { range = 0 }
    }


characters0 : List Character
characters0 =
    [ alice, bob, eve ]


metadata0 : LevelMetadata
metadata0 =
    { name = "rendering demo level"
    , id = -1
    }


lvlDesc : LevelDescription
lvlDesc =
    LevelDescription
        boundaries0
        obstacles0
        characters0
        {}
        metadata0


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
    , currentLevel = lvlDesc
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



-- No update for now...


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- No subscriptions for now...


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
