module Main exposing (..)

import Definitions exposing (..)
import Render exposing (..)
import Html exposing (..)
import Html.Events exposing (on)
import Mouse as Mouse
import Time exposing (Time, millisecond)
import Json.Decode as Decode
import Levels exposing (level1)
import List.Extra as ListE


selectTolerance : Float
selectTolerance =
    20


initToolsState : ToolDefinitions -> ToolsState
initToolsState definitions =
    List.indexedMap
        (\i def ->
            { id = i
            , name = def.name
            , position = def.startPosition
            , toolType = def.toolType
            , nodeParameters = def.nodeParameters
            , active = False
            }
        )
        definitions


initModel : Model
initModel =
    { levels = []
    , currentLevel = level1
    , levelState = { time = 0, progress = Started, tools = initToolsState level1.tools }
    , gameState = { currentLevel = -1, progress = PlayingLevel (-1) }
    , drag = Nothing
    , canvasSize = { width = 700, height = 600 }
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [ onMouseDown model.canvasSize ]
        [ renderModel model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseEvent event ->
            ( doMouseEvent event model, Cmd.none )

        _ ->
            ( model, Cmd.none )


doMouseEvent : MouseEvent -> Model -> Model
doMouseEvent event model =
    case event of
        DragStart startPosition ->
            { model | drag = pickUpTool startPosition model.levelState.tools }

        DragAt position ->
            { model | levelState = updateToolPosition position model.levelState model.drag }

        DragEnd position ->
            { model
                | drag = Nothing
                , levelState = updateToolDropPosition position model
            }


pickUpTool : Position -> ToolsState -> Maybe Drag
pickUpTool pos tools =
    let
        clickedTool =
            hasClickedOnTool pos tools

        toDrag tool =
            { start = pos, current = pos, tool = tool }
    in
        Maybe.map toDrag clickedTool


hasClickedOnTool : Position -> ToolsState -> Maybe Tool
hasClickedOnTool pos tools =
    List.head <| List.filter (isInPosition pos) tools


isInPosition : Position -> Tool -> Bool
isInPosition pos tool =
    abs (pos.x - tool.position.x) < selectTolerance && abs (pos.y - tool.position.y) < selectTolerance


updateToolPosition : Position -> LevelState -> Maybe Drag -> LevelState
updateToolPosition mousePosition state drag =
    case drag of
        Nothing ->
            state

        Just dragData ->
            let
                tool =
                    dragData.tool

                updatedTools =
                    List.map
                        (\t ->
                            if t.id == tool.id then
                                { t | position = mousePosition }
                            else
                                t
                        )
                        state.tools
            in
                { state | tools = updatedTools }


updateToolDropPosition : Position -> Model -> LevelState
updateToolDropPosition mousePosition model =
    let
        levelState =
            model.levelState
    in
        case model.drag of
            Nothing ->
                levelState

            Just { tool } ->
                if isValidDropPosition mousePosition model then
                    let
                        updatedTools =
                            updateToolInTools
                                tool
                                (\t -> { t | position = mousePosition, active = True })
                                levelState.tools
                    in
                        { levelState | tools = updatedTools }
                else
                    let
                        updatedTools =
                            updateToolInTools
                                tool
                                (\t -> { t | position = getOriginalToolPosition model.currentLevel t, active = False })
                                levelState.tools
                    in
                        { levelState | tools = updatedTools }


updateToolInTools : Tool -> (Tool -> Tool) -> ToolsState -> ToolsState
updateToolInTools tool update tools =
    List.map
        (\t ->
            if t.id == tool.id then
                update t
            else
                t
        )
        tools


isValidDropPosition : Position -> Model -> Bool
isValidDropPosition position model =
    let
        inBoundingBox pos boundary =
            (pos.x >= boundary.leftBottom.x)
                && (pos.x <= boundary.leftBottom.x + boundary.dimensions.width)
                && (pos.y >= boundary.leftBottom.y)
                && (pos.y <= boundary.leftBottom.y + boundary.dimensions.height)

        collidesWith : Position -> List Boundaries -> Bool
        collidesWith pos boundaries =
            List.any (inBoundingBox pos) boundaries
    in
        ((inBoundingBox position model.currentLevel.boundaries)
            && (not <| collidesWith position model.currentLevel.obstacles)
        )


getOriginalToolPosition : LevelDescription -> Tool -> Position
getOriginalToolPosition level tool =
    let
        definition =
            ListE.find (\t -> t.name == tool.name) level.tools
    in
        case definition of
            Nothing ->
                Debug.crash ("Tool " ++ tool.name ++ " must have a definition")

            Just def ->
                def.startPosition


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Time.every millisecond Tick

        --            Sub.none
        Just _ ->
            Sub.batch
                [ Mouse.moves (MouseEvent << DragAt << mousePos2Pos model.canvasSize)
                , Mouse.ups (MouseEvent << DragEnd << mousePos2Pos model.canvasSize)
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


mousePos2Pos : GameDimensions -> Mouse.Position -> Position
mousePos2Pos game p =
    Position (toFloat p.x - (game.width / 2)) ((game.height / 2) - toFloat p.y)


onMouseDown : GameDimensions -> Attribute Msg
onMouseDown game =
    on "mousedown" (Decode.map (MouseEvent << DragStart << mousePos2Pos game) Mouse.position)
