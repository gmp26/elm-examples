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

grain : Int
grain = 24

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

type Strip     =  { color : Color
                  , loc : Location
                  , n : Int
                  , v : (Int, Int)
                  }

type GameState =  [Strip]

data State = Start | Play GameState | Over

type Location = (Int, Int) 

initialGameState =  [ {color = red, loc = (-40,0), n = 3, v = (0,3)}
                    , {color = green, loc = (0,0), n = 6, v = (0,2)}
                    , {color = blue, loc = (40,0), n = 5, v = (0,1)}  
                    ]

initialState = Start

-- UPDATE

data Event = Drop | GotoPlay

moveStrip : Strip -> Strip
moveStrip s = 
  if hitBottom 1000 (snd s.loc) s.n
    then {s | v <- (0, 0)}
    else {s | loc <- (fst s.loc + fst s.v, snd s.loc + snd s.v) }

drop : State -> State
drop s = case s of
            Play gs -> Play (map moveStrip gs)
            otherwise -> s

update : Event -> State -> State
update event s = case event of
  GotoPlay      -> Play initialGameState
  Drop          -> drop s
  otherwise     -> initialState

-- DISPLAY

-- one coloured hexagon
hex : Color -> Int -> Form
hex col h = ngon 6 (size h)
               |> filled col  

-- height of hexagon
hexH : Int -> Float
hexH h = (size h) * 1.8

hexW : Int -> Float
hexW h = (size h) * 2

-- the jth coloured hexagon in a strip
hexAt : Color -> Int -> Int -> Form
hexAt col h j = ngon 6 (size h)
              |> filled col
              |> moveY -(tf j * (hexH h))

hotSpot :  Int -> Int -> Element
hotSpot h n = node "div"
                    [ "className" := "hotspot"]
  
                    [ "width"     := px (hexW h)
                    , "height"    := px ((hexH h) * tf n)
                    , "cursor"    := "pointer"
                    , "color"     := "white"
                    , "textAlign" := "center"
                    , "verticalAlign" := "middle"
                    , "fontSize"  := px 30
                    , "border"    := "1px solid darkGrey"
                    , "boxSizing" := "border-box"
                    ]
  
                    [ text <| show n ]

                  |> toElement (round <| (hexW h)) (round <| (hexH h))

hexStripForm : Int -> Color -> Int -> Form
hexStripForm h col n =
  let hexunit = hexAt col h
  in map hexunit [0..(n-1)]
        |> group

hHEIGHT : Int -> Int -> Int
hHEIGHT h n = (round <| (hexH h))*n

hHeight : Int -> Int -> Float
hHeight h n = (hexH h)*(tf n)

hexStripElement : Int -> Color -> Int -> Element
hexStripElement h col n = 
  let hh = (round <| (hexH h))*n
      hw = (round <| (hexW h))
  in flow outward   [ collage hw hh
                      <|  [ hexStripForm h col n 
                              |> moveY  ((tf hh - hexH h)/ 2)
                          ]
                    , hotSpot h n
                    ]

positionedStrip : Color -> Int -> (Int, Int) -> Location -> Element
positionedStrip col n (w, h) (x,y) =
  let 
    xpos = absolute (x + w // 2)
    ypos = absolute y
    position = midTopAt xpos ypos
  in hexStripElement h col n
     |> container w h position

hitBottom : Int -> Int -> Int -> Bool
hitBottom h y n = 
  y >= (h - watch "hHEIGHT" hHEIGHT (watch "h" h) (watch "n" n))

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
--  case (watch "state" state) of
  case (state) of
    Start   ->  startScreen
    Play gamestate   ->  stage w h gamestate 
    otherwise -> asText "Not defined" 

-- PLUMBING

startClick : Signal Event
startClick = (always GotoPlay)     <~ Mouse.clicks  

dropSignal : Signal Event
dropSignal = (always Drop) <~ fps 60

playSignal : Signal Event
playSignal = merge startClick dropSignal

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState playSignal

--main = asText <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)
