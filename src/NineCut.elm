module NineCut exposing (..)

import Collage exposing (Form, group, groupTransform, toForm)
import Element
import Transform


type alias SpriteCuts =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    , width : Int
    , height : Int
    , image : String
    }


renderNineCutSprite : SpriteCuts -> Int -> Int -> Form
renderNineCutSprite cuts width height =
    let
        container =
            Element.container width height

        xRatio =
            toFloat cuts.width / toFloat width

        yRatio =
            toFloat cuts.height / toFloat height

        crop left right top bottom =
            Element.croppedImage ( left, top ) (right - left) (bottom - top) cuts.image

        topLeft =
            crop 0 cuts.left 0 cuts.top

        topMiddle =
            crop cuts.left cuts.right 0 cuts.top

        topRight =
            crop cuts.right cuts.width 0 cuts.top

        middleLeft =
            crop 0 cuts.left cuts.top cuts.bottom

        middle =
            crop cuts.left cuts.right cuts.top cuts.bottom

        middleRight =
            crop cuts.right cuts.width cuts.top cuts.bottom

        bottomLeft =
            crop 0 cuts.left cuts.bottom cuts.height

        bottomMiddle =
            crop cuts.left cuts.right cuts.bottom cuts.height

        bottomRight =
            crop cuts.right cuts.width cuts.bottom cuts.height

        topCutOffset =
            Element.absolute <| round (toFloat cuts.top / yRatio)

        bottomCutOffset =
            Element.absolute <| round (toFloat cuts.bottom / yRatio)

        leftCutOffset =
            Element.absolute <| round (toFloat cuts.left / xRatio)

        rightCutOffset =
            Element.absolute <| round (toFloat cuts.right / xRatio)

        scale =
            Transform.multiply
                (Transform.scaleX (xRatio))
                (Transform.scaleY (yRatio))
    in
        group <|
            List.map
                (toForm >> List.singleton >> groupTransform (Transform.scaleX xRatio))
                [ container Element.middle middle
                , container Element.topLeft topLeft
                , container (Element.midTop) topMiddle
                , container (Element.topLeftAt rightCutOffset (Element.absolute 0)) topRight
                , container (Element.topLeftAt (Element.absolute 0) (topCutOffset)) middleLeft
                , container (Element.topLeftAt rightCutOffset topCutOffset) middleRight
                , container (Element.topLeftAt (Element.absolute 0) bottomCutOffset) bottomLeft
                , container (Element.topLeftAt leftCutOffset bottomCutOffset) bottomMiddle
                , container (Element.topLeftAt rightCutOffset bottomCutOffset) bottomRight
                ]
