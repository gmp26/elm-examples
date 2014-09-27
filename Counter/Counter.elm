

import Html
import Html (..)
import Window
import Debug as D

margin = 100

width' w = toFloat (w - 2 * margin)
height' h = toFloat (h - 2 * margin)

counter : Float -> Int -> Int -> Html
counter count w h =
    node "div" []
        [ "textAlign"     := "center"
        , "verticalAlign" := "center"
        , "lineHeight"    := px << toFloat <| h // 2  - margin
        , "border"        := "50px dashed purple"
        , "borderRadius"  := px 50
        , "margin"        := px margin
        , "width"         := px << width' <| w
        , "height"        := px << height' <| h
        , "background"    := "#98f"
        , "color"         := "#0ff"
        , "fontSize"      := px 50
        , "cursor"        := "pointer"
        , "boxSizing"     := "border-box"
        ]
        [ text (show <| count) ]

scene : Int -> (Int,Int) -> Element
scene count (w,h) =
    Html.toElement w h (counter (toFloat count) w h)

main = scene <~ count (every second) ~ ((D.watch "(w,h)=") <~ Window.dimensions)