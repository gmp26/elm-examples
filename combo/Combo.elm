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

-- a vertical strip of hexagons labelled with length
hotSpot : Float -> Int -> Form 
hotSpot size n =  node "div"
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
                  |> toForm


hexStrip : Color -> Float -> Int -> (Int, Int) -> Form
hexStrip col size n (w,h) =
  let hexunit = hexAt col size
      --label   = n
      --          |> show
      --          |> T.toText
      --          |> T.typeface ["Helvetica Neue", "arial", "sans-serif"]
      --          |> T.height size
      --          |> T.bold
      --          |> T.color white
      --          |> T.centered
      --          |> toForm
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
            ]

startScreen : Element
startScreen = [markdown| 
# Click to start
|]

render : (Int, Int) -> State -> Element
render (w,h) screen = case screen of
  Start   ->  startScreen
  Play    ->  stage w h
  otherwise -> asText "Not defined" 

-- PLUMBING

startClick = (always GotoPlay)     <~ Mouse.clicks  

main = render <~ Window.dimensions ~ foldp update initialState startClick
--main = asText <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)
