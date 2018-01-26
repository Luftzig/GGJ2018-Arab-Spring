module Definitions exposing (..)

import Time exposing (Time)
import Time exposing (Time)
import Mouse as Mouse

type alias Model =
    { levels : List LevelDescription
    , currentLevel : LevelDescription
    , levelState : LevelState
    , gameState : GameState
    , drag : Maybe Drag
    }

type Msg
    = Tick Time
    | MouseEvent MouseEvent

type MouseEvent
    = DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position

type alias Drag =
    { start : Position
    , current : Position
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
    { leftBottom : Position
    , dimensions : Dimensions
    }


type alias Position =
    { x : Float
    , y : Float
    }


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
    List Tool


type alias Tool =
    { name : String
    , toolType : ToolType
    , nodeParameters : NodeParameters
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
