module Levels exposing (..)

import Definitions exposing (Boundaries, Box, Character, CharacterRole(Alice, Bob, Eve), LevelDescription, LevelMetadata, Obstacle)


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


level1 : LevelDescription
level1 =
    LevelDescription
        boundaries0
        obstacles0
        characters0
        {}
        metadata0
