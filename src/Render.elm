module Render exposing (..)

import Definitions exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element
import Html exposing (..)


cornerToCenter : Box -> Position
cornerToCenter b =
    { x = b.leftBottom.x + b.dimensions.width / 2
    , y = b.leftBottom.y + b.dimensions.height / 2
    }

posToTuple : Position -> (Float, Float)
posToTuple p = (p.x, p.y)

renderBoundaries : Boundaries -> Form
renderBoundaries obj = rect obj.dimensions.width obj.dimensions.height
    |> filled yellow
    |> move (cornerToCenter obj |> posToTuple)

renderObstacle : Obstacle -> Form
renderObstacle obj = rect obj.dimensions.width obj.dimensions.height
    |> filled blue
    |> move (cornerToCenter obj |> posToTuple)

renderCharacter : Character -> Form
renderCharacter obj = circle 25
    |> filled red
    |> move (obj.position.x, obj.position.y)

renderToolbox : Boundaries -> Form
renderToolbox obj = rect obj.dimensions.width 100
    |> filled green
    |> move (cornerToCenter obj |> posToTuple)
    |> moveY (-obj.dimensions.height / 2 - 50)

renderTool : Tool -> Form
renderTool obj = ngon 3 30
    |> filled red
    |> move (obj.position.x, obj.position.y)

renderModel : Model -> Html Msg
renderModel model = Element.toHtml <| collage
    (round model.currentLevel.boundaries.dimensions.width)
    (round model.currentLevel.boundaries.dimensions.height + 200)
    ( renderBoundaries model.currentLevel.boundaries
    :: renderToolbox model.currentLevel.boundaries
    :: List.map renderObstacle model.currentLevel.obstacles
    ++ List.map renderCharacter model.currentLevel.characters
    ++ List.map renderTool model.levelState.tools
    )
