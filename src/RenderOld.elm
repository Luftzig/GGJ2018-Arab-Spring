module Render exposing (..)

import Definitions exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element
import Html exposing (..)
import Paths exposing (Edge, makeEdges)
import RayCasting exposing (rayAngle, rayCasting, raysPolygons)
import Text
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
    Element.image (round obj.box.dimensions.width) (round obj.box.dimensions.height) obj.background
        |> toForm
        --        |> filled yellow
        |> move (cornerToCenter obj.box |> posToTuple)


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
            group
                [ Element.fittedImage 64 64 "assets/router1.png"
                    |> toForm
                , Collage.text (Text.fromString obj.name |> Text.height 14 |> Text.color darkRed) |> move ( -12, 24 )
                ]
                |> move ( obj.position.x, obj.position.y )

        Bob ->
            group
                [ Element.fittedImage 64 64 "assets/router1.png"
                    |> toForm
                , Collage.text (Text.fromString obj.name |> Text.height 14 |> Text.color darkRed) |> move ( -12, 24 )
                ]
                |> move ( obj.position.x, obj.position.y )

        Eve ->
            group
                [ Element.fittedImage 64 64 "assets/eve_truck.png"
                    |> toForm
                , Collage.text (Text.fromString obj.name |> Text.height 14 |> Text.color darkRed) |> move ( -12, 24 )
                ]
                |> move ( obj.position.x, obj.position.y )


renderToolbox : Boundaries -> Form
renderToolbox obj =
    rect obj.box.dimensions.width 100
        |> filled green
        |> move (cornerToCenter obj.box |> posToTuple)
        |> moveY (-obj.box.dimensions.height / 2 - 50)


renderTool : Tool -> Form
renderTool obj =
    case obj.toolType of
        Repeater ->
            group
                [ Element.fittedImage 64 64 "assets/repeater2.png"
                    |> toForm
                , Collage.text (Text.fromString obj.name |> Text.height 14 |> Text.color darkRed) |> move ( 0, 32 )
                ]
                |> move ( obj.position.x, obj.position.y )


renderEdges : Color -> List Edge -> List Form
renderEdges color edges =
    List.map
        (\e ->
            segment ( e.start.x, e.start.y ) ( e.end.x, e.end.y )
                |> traced { color = color, width = 2, cap = Round, join = Smooth, dashing = [ 3, 3 ], dashOffset = 0 }
        )
        edges


renderAreasOfEffect : Boundaries -> List Obstacle -> Color -> List (Node a) -> List Form
renderAreasOfEffect boundaries obstacles color nodes =
    nodes
        |> List.map (renderRays boundaries obstacles color)
        |> List.concat


renderRays : Boundaries -> List Obstacle -> Color -> Node a -> List Form
renderRays boundaries obstacles color node =
    rayCasting (Just ( node.node.range, 50 )) node.position boundaries.box obstacles
        |> List.sortBy rayAngle
        |> raysPolygons
        |> List.map (filled color >> alpha 0.5)


renderModel : Model -> Html Msg
renderModel model =
    Element.toHtml <|
        collage
            (round model.canvasSize.width)
            (round model.canvasSize.height)
            (renderBoundaries model.currentLevel.boundaries
                :: List.map renderObstacle model.currentLevel.obstacles
                ++ renderAreasOfEffect
                    model.currentLevel.boundaries
                    model.currentLevel.obstacles
                    lightOrange
                    (List.filter .active model.levelState.tools)
                ++ renderAreasOfEffect
                    model.currentLevel.boundaries
                    model.currentLevel.obstacles
                    lightPurple
                    (List.filter isAdversary model.currentLevel.characters)
                ++ renderAreasOfEffect
                    model.currentLevel.boundaries
                    model.currentLevel.obstacles
                    lightBlue
                    (List.filter (not << isAdversary) model.currentLevel.characters)
                ++ List.map renderCharacter model.currentLevel.characters
                ++ [ renderToolbox model.currentLevel.boundaries ]
                ++ List.map renderTool model.levelState.tools
                ++ (renderEdges lightGreen <|
                        makeEdges
                            model.currentLevel.obstacles
                            model.currentLevel.characters
                            (List.filter .active model.levelState.tools)
                   )
                ++ (renderEdges lightOrange <|
                        makeEdges
                            model.currentLevel.obstacles
                            (List.filter .active model.levelState.tools)
                            (List.filter .active model.levelState.tools)
                   )
            )
