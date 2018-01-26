module Definitions exposing (..)

import Time exposing (Time)

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
