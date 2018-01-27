module Levels exposing (..)

import Definitions exposing (Boundaries, Box, Character, CharacterRole(Alice, Bob, Eve), LevelDescription, LevelMetadata, Obstacle, ToolDefinition, ToolDefinitions, ToolType(Repeater))


boundaries0 : Boundaries
boundaries0 =
    { box =
        { leftBottom = { x = -350, y = -200 }
        , dimensions = { width = 700, height = 400 }
        }
    , background = "assets/level1.png"
    }


obstacles0 : List Obstacle
obstacles0 =
    [ Box { x = -350, y = -100 } { width = 300, height = 200 }
    , Box { x = 50, y = -100 } { width = 100, height = 200 }
    , Box { x = -350, y = 165 } { width = 140, height = 70 }
    , Box { x = -200, y = 165 } { width = 171, height = 90 }
    , Box { x = -23, y = 165 } { width = 173, height = 78 }
    , Box { x = (648 - 350), y = (200 - (-40) - 87) } { width = 93, height = 87 }
    , Box { x = (648 - 350), y = (200 - 55 - 137) } { width = 110, height = 137 }
    , Box { x = (641 - 350), y = (200 - 200 - 137) } { width = 103, height = 137 }
    , Box { x = (648 - 350), y = (200 - 391 - 100) } { width = 103, height = 100 }
    , Box { x = (428 - 350), y = (200 - 385 - 100) } { width = 86, height = 100 }
    , Box { x = (226 - 350), y = (200 - 392 - 98) } { width = 198, height = 98 }
    , Box { x = (99 - 350), y = (200 - 384 - 98) } { width = 122, height = 98 }
    , Box { x = (-41 - 350), y = (200 - 388 - 100) } { width = 138, height = 100 }
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


level1Tools : ToolDefinitions
level1Tools =
    [ { name = "Cheap Repeater"
      , toolType = Repeater
      , nodeParameters = { range = 100 }
      , startPosition = { x = -250, y = -250 }
      }
    ]


level1 : LevelDescription
level1 =
    LevelDescription
        boundaries0
        obstacles0
        characters0
        level1Tools
        metadata0
