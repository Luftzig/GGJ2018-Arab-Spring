module Definitions exposing (..)

import Time exposing (Time)
import Mouse as Mouse


type alias Model =
    { levels : List LevelDescription
    , currentLevel : LevelDescription
    , levelState : LevelState
    , gameState : GameState
    , drag : Maybe Drag
    , canvasSize : GameDimensions
    }


type alias GameDimensions =
    Dimensions


type Msg
    = Tick Time
    | MouseEvent MouseEvent


type MouseEvent
    = DragStart Position
    | DragAt Position
    | DragEnd Position


type alias Drag =
    { start : Position
    , current : Position
    , tool : Tool
    }


type alias LevelDescription =
    { boundaries : Boundaries
    , obstacles : List Obstacle
    , characters : List Character
    , tools : ToolDefinitions
    , metadata : LevelMetadata
    }


type alias Boundaries =
    { box : Box
    , background : String
    }


type alias Box =
    { leftBottom : Position
    , dimensions : Dimensions
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Positioned a =
    { a | position : Position }


type alias Dimensions =
    { width : Width
    , height : Height
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
    , position : Position
    , node : NodeParameters
    }


type CharacterRole
    = Alice
    | Bob
    | Eve


isSource : Character -> Bool
isSource char =
    case char.role of
        Alice ->
            True

        _ ->
            False


isAdversary : Character -> Bool
isAdversary char =
    case char.role of
        Eve ->
            True

        _ ->
            False


isTarget : Character -> Bool
isTarget char =
    case char.role of
        Bob ->
            True

        _ ->
            False


type alias NodeParameters =
    { range : RadioRange
    }


type alias HasNode a =
    { a | node : NodeParameters }


type alias Node a =
    HasNode (Positioned a)


type alias RadioRange =
    Float


type alias ToolDefinitions =
    List ToolDefinition


type alias ToolDefinition =
    { name : String
    , startPosition : Position
    , toolType : ToolType
    , nodeParameters : NodeParameters
    }


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
    List Tool


type alias Tool =
    { name : String
    , id : Id
    , toolType : ToolType
    , node : NodeParameters
    , position : Position
    , active : Bool
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
