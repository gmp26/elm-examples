module Scaling where

import Html (Html, toElement, style, prop)
import Html.Tags (div, img)
import Html.Attributes (src)

scene : Int -> Element
scene n =
    toElement n n (yogi n)

yogi : Int -> Html
yogi n =
    div [ style
            [ prop "width" "100%"
            , prop "height" "100%"
            , prop "backgroundColor" "red"
            , prop "transform" ("rotate3d(1, 1, 1, " ++ (show n) ++ "deg)")
            , prop "overflow-y" "scroll"
            ]
        ]
        [
            img [ src "http://elm-lang.org/yogi.jpg"
                , style
                    [ prop "width" "100px"
                    , prop "height" "200%"
                    ]
                ]
                []
        ]

-- SIGNALS

main : Signal Element
main =
    scene <~ size

size : Signal Int
size =
    fps 30
        |> foldp (+) 0
        |> lift (\t -> round (200 + 100 * sin (degrees t / 10)))
