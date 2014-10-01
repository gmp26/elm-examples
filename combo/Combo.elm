module Combo where

import Debug (watch)
import Mouse
import Window
import Random
import Text as T
import Html
import Html (Html, node, toElement, (:=), px, text)

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

type Strip     =  { color : Color
                  , loc : Location
                  , n : Int
                  , v : (Int, Int)
                  }

type GameState =  [Strip]

data State = Start | Play GameState | Over

type Location = (Int, Int)

type LaunchPad = [Int] 

initialGameState =  [ {color = red, loc = (-40,0), n = 3, v = (0,1)}
                    , {color = green, loc = (0,0), n = 6, v = (0,0)}
                    , {color = blue, loc = (40,0), n = 5, v = (0,0)}  
                    ]

initialState = Start

-- UPDATE

data Event = Drop | GotoPlay

moveStrip : Strip -> Strip
moveStrip s = 
  if hitBottom 300 (snd s.loc) 3 20
    then {s | v <- (0, 0)}
    else {s | loc <- (fst s.loc + fst s.v, snd s.loc + snd s.v) }

drop : State -> State
drop s = case s of
            Play gs -> Play (map moveStrip gs)
            otherwise -> s

update : Event -> State -> State
update event s = case event of
  GotoPlay    -> Play initialGameState
  Drop        -> drop s
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

hotSpot : Float -> Int -> Element
hotSpot size n = node "div"
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

hexStripForm : Color -> Float -> Int -> Form
hexStripForm col size n =
  let hexunit = hexAt col size
  in map hexunit [0..(n-1)]
        |> group

hHEIGHT : Float -> Int -> Int
hHEIGHT size n = (round <| hexH size)*n

hHeight : Float -> Int -> Float
hHeight size n = (hexH size)*(tf n)

hexStripElement : Color -> Float -> Int -> Element
hexStripElement col size n = 
  let hh = (round <| hexH size)*n
      hw = (round <| hexW size)
  in flow outward  [ collage hw hh
                    <| [ hexStripForm col size n
                          |> moveY  ((tf hh - hexH size)/ 2)
                        ]
                , hotSpot size n
                ]

positionedStrip : Color -> Int -> (Int, Int) -> Location -> Element
positionedStrip col n (w, h) (x,y) =
  let 
    xpos = absolute (x + w // 2)
    ypos = absolute y
    position = midTopAt xpos ypos
  in hexStripElement col 20 n
     |> container w h position

hitBottom : Int -> Int -> Int -> Float -> Bool
hitBottom h y n size = 
  y >= (h - hHEIGHT size n)

drawStrip : Int -> Int -> Strip -> Element
drawStrip w h strip = positionedStrip strip.color strip.n (w,h) strip.loc

stage : Int -> Int -> GameState -> Element
stage w h gamestate = layers <| map (drawStrip w h) gamestate 

startScreen : Element
startScreen = [markdown| 
# Click to start
|]

render : (Int, Int) -> State -> Element
render (w,h) state =
  case (watch "state" state) of
    Start   ->  startScreen
    Play gamestate   ->  stage w h gamestate 
    otherwise -> asText "Not defined" 

-- PLUMBING

startClick : Signal Event
startClick = (always GotoPlay)     <~ Mouse.clicks  

dropSignal : Signal Event
dropSignal = (always Drop) <~ fps 30

playSignal : Signal Event
playSignal = merge startClick dropSignal

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState playSignal

--main = asText <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)
