module Main exposing (..)

import Html exposing (..)
import Time exposing (Time)


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { levels : List LevelDescription
    , currentLevel : LevelDescription
    , levelState : LevelState
    , gameState : GameState
    }


type alias LevelDescription =
    { boundaries : Boundaries
    , obstacles : List Obstacle
    , characters : List Character
    , tools : ToolsRestrictions
    , metadata : LevelMetadata
    }


type alias Boundaries =
    Box


type alias Box =
    { topLeft : Position
    , dimensions : Dimensions
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Dimensions =
    { height : Height
    , width : Width
    }


type alias Height =
    Float


type alias Width =
    Float


type alias Obstacle =
    Box


type alias Character =
    { role : CharacterRole
    , name : String
    , location : Position
    , node : NodeParameters
    }


type CharacterRole
    = Alice
    | Bob
    | Eve


type alias NodeParameters =
    { range : RadioRange
    }


type alias RadioRange =
    Float


type alias ToolsRestrictions =
    {}


type alias LevelMetadata =
    { name : String
    , id : Id
    }


type alias Id =
    Int


type alias LevelState =
    { time : Time
    , progress : LevelProgress
    , tools : ToolsState
    }


type LevelProgress
    = Started
    | Succeeded
    | Failed


type alias ToolsState =
    {}


type alias Tool =
    { name : String
    , toolType : ToolType
    , nodeParameters : NodeParameters
    }


type ToolType
    = Repeater


type alias GameState =
    { currentLevel : Id
    , progress : GameProgress
    }


type GameProgress
    = Menu
    | Prologue
    | PlayingLevel Id
    | Paused
    | Epilogue


initialModel : Model
initialModel =
    {}


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, world!"
        , text (toString model)
        ]
