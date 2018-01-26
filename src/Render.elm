module Render exposing (..)

import Definitions exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element
import Html exposing (..)
import Transform


cornerToCenter : Box -> Position
cornerToCenter b =
    { x = b.leftBottom.x + b.dimensions.width / 2
    , y = b.leftBottom.y + b.dimensions.height / 2
    }


posToTuple : Position -> ( Float, Float )
posToTuple p =
    ( p.x, p.y )


renderBoundaries : Boundaries -> Form
renderBoundaries obj =
    rect obj.dimensions.width obj.dimensions.height
        |> filled yellow
        |> move (cornerToCenter obj |> posToTuple)


renderObstacle : Obstacle -> Form
renderObstacle obj =
    -- Strange voodoo below: using stretchedImage not in a group yields wrong result
    group
        [ stretchedImage { width = 256, height = 256 } obj.dimensions "assets/building.png"
        ]
        |> move (cornerToCenter obj |> posToTuple)


stretchedImage : Dimensions -> Dimensions -> String -> Form
stretchedImage imageSize targetSize image =
    let
        xRatio =
            imageSize.width / targetSize.width

        yRatio =
            imageSize.height / targetSize.height
    in
        groupTransform
            (Transform.multiply (Transform.scaleX <| 1 / xRatio) (Transform.scaleY <| 1 / yRatio))
            [ toForm <| Element.image (round imageSize.width) (round imageSize.height) image ]


renderCharacter : Character -> Form
renderCharacter obj =
    case obj.role of
        Alice ->
            Element.fittedImage 40 40 "assets/router1.png"
                |> toForm
                |> move ( obj.position.x, obj.position.y )

        Bob ->
            Element.fittedImage 40 40 "assets/router1.png"
                |> toForm
                |> move ( obj.position.x, obj.position.y )

        Eve ->
            Element.fittedImage 40 40 "assets/eve_truck.png"
                |> toForm
                |> move ( obj.position.x, obj.position.y )


renderToolbox : Boundaries -> Form
renderToolbox obj =
    rect obj.dimensions.width 100
        |> filled green
        |> move (cornerToCenter obj |> posToTuple)
        |> moveY (-obj.dimensions.height / 2 - 50)


renderTool : Tool -> Form
renderTool obj =
    case obj.toolType of
        Repeater ->
            Element.fittedImage 40 40 "assets/repeater2.png"
                |> toForm
                |> move ( obj.position.x, obj.position.y )


renderModel : Model -> Html Msg
renderModel model =
    Element.toHtml <|
        collage
            (round model.canvasSize.width)
            (round model.canvasSize.height)
            (renderBoundaries model.currentLevel.boundaries
                :: renderToolbox model.currentLevel.boundaries
                :: List.map renderObstacle model.currentLevel.obstacles
                ++ List.map renderCharacter model.currentLevel.characters
                ++ List.map renderTool model.levelState.tools
            )
