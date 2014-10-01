module Combo where

import Mouse
import Window
import Random
import Text as T
import Html
import Html (Html, node, toElement, (:=), px, text)
import Debug

-- CONFIG

-- HELPER FUNCTIONS

tf = toFloat

relativeMouse : Location -> Location -> Location
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : Location -> Location
center (w, h) = (w // 2, h // 2)

-- INPUT
delta = fps 30


--input = (,) <~ lift inSeconds delta
--             ~ sampleOn delta (lift2 relativeMouse (lift center Window.dimensions) Mouse.position)

-- MODEL

data State = Start | Play | Over

type Style = { background : Color }

type Location = (Int, Int)
type Flocation = (Float, Float)

type Strip a =  { a | length : Int
                --, pos : Location
                --, color : Color
                --, class : Style
                }

type LaunchPad = [Int] 

initialState = Start

-- UPDATE

data Event = Tick Time | GotoPlay

update : Event -> State -> State
update event s = case event of
  GotoPlay    -> Play
  otherwise   -> initialState

-- DISPLAY

-- one coloured hexagon
hex : Color -> Float -> Form
hex col size = ngon 6 size
               |> filled col  

-- height of hexagon
hexH : Float -> Float
hexH size = size * 1.8

hexW : Float -> Float
hexW size = size * 2

-- the jth coloured hexagon in a strip
hexAt : Color -> Float -> Int -> Form
hexAt col size j = ngon 6 size
              |> filled col
              |> moveY -(tf j * hexH size)

hotSpotElement : Float -> Int -> Element
hotSpotElement size n = node "div"
                    [ "className" := "hotspot"]
  
                    [ "width"     := px (hexW size)
                    , "height"    := px (hexH size * tf n)
                    , "cursor"    := "pointer"

                    , "color"     := "white"
                    , "textAlign" := "center"
                    , "fontSize"  := px 30
                    , "border"    := "1px solid darkGrey"
                    , "boxSizing" := "border-box"
                    ]
  
                    [ text <| show n ]

                  |> toElement (round <| hexW size) (round <| hexH size)

-- a vertical strip of hexagons labelled with length in a Form
hotSpot : Float -> Int -> Form 
hotSpot size n =  hotSpotElement size n
                  |> toForm


hexStrip : Color -> Float -> Int -> (Int, Int) -> Form
hexStrip col size n (w,h) =
  let hexunit = hexAt col size
  in map hexunit [0..(n-1)] ++ [hotSpot size n]
        |> group
        |> moveY ((tf h - hexH size) / 2)


stage : Int -> Int -> Element
stage w h = collage w h
            [ hexStrip red 20 6 (w,h)
            , hexStrip orange 20 4 (w,h)
                |> move (40, 0)            
            , hexStrip lightBlue 20 3 (w,h)
                |> move (-40, 0)
            , hexStrip yellow 20 7 (w,h)
                |> move (-80, 0)
            , circle 20
                |> filled red
            ]


hexStripTrimmed : Color -> Float -> Int -> Form
hexStripTrimmed col size n =
  let hexunit = hexAt col size
  in map hexunit [0..(n-1)]
        |> group

hexStripElement : Color -> Float -> Int -> Element
hexStripElement col size n = 
  let hh = (round <| hexH size)*n
      hw = (round <| hexW size)
  in flow outward  [ collage hw hh
                    <| [ hexStripTrimmed col size n
                          |> moveY  ((tf hh - hexH size)/ 2)
                        ]
                , hotSpotElement size n
                ]

positionStrip : Color -> (Int, Int) -> Location -> Element
positionStrip col (w, h) (x,y) =
  let 
    xpos = absolute x
    ypos = absolute y
    position = middleAt xpos ypos
  in hexStripElement col 20 6
     |> container w h position


stageFromElements : Int -> Int -> Element
stageFromElements w h =
  layers [
    positionStrip orange (w, h) (0,0)
  ]

startScreen : Element
startScreen = [markdown| 
# Click to start
|]

render : (Int, Int) -> State -> Element
render (w,h) state = case state of
  Start   ->  startScreen
  Play    ->  stageFromElements w h --stage w h
  otherwise -> asText "Not defined" 

-- PLUMBING

startClick = (always GotoPlay)     <~ Mouse.clicks  

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState startClick
--main = asText <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)
