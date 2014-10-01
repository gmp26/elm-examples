module Combo where

import Debug (watch)
import Mouse
import Window
import Random
import Text as T
import Html
import Html (Html, node, toElement, (:=), px, text)

-- HELPERS

tf : Int -> Float
tf = toFloat

--relativeMouse : Location -> Location -> Location
--relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

--center : Location -> Location
--center (w, h) = (w // 2, h // 2)

-- INPUT

-- MODEL

grain : Int
grain = 24

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

type Strip     =  { i : Int
                  , color : Color
                  , loc : Location
                  , n : Int
                  , v : (Int, Int)
                  }

type GameState    =   [Strip]
data ScreenState  =   Start | Play GameState | GameOver
type State        =   { screen  : ScreenState
                      , width   : Int
                      , height  : Int
                      }

type Location = (Int, Int) 

initialGameState =  [ {i = -1, color = red, loc = (-40,0), n = 3, v = (0,3)}
                    , {i = 0, color = green, loc = (0,0), n = 6, v = (0,4)}
                    , {i = 1, color = blue, loc = (40,0), n = 5, v = (0,5)}  
                    ]

initialState =  { screen = Start
                , width = 320
                , height = 480
                }

-- UPDATE

data Event = Drop | GotoPlay | Resize Int Int

moveStrip : Int -> Int -> Strip -> Strip
moveStrip w h s = 
  if hitBottom h (snd s.loc) s.n
    then  { s | v <- (0, 0)
              , loc <- (fst s.loc, h - s.n*(round <| hexH h))
          }
    else  { s | loc <- (fst s.loc + fst s.v, snd s.loc + snd s.v) }

drop : State -> State
drop s =
  let {screen, width, height} = s
  in case screen of
    Play gs -> {s | screen <- Play (map (moveStrip s.width s.height) gs) }
    otherwise -> s

-- adjust horizontal separation between strips so it's about the width of a block
adjustSeparation : Int -> Strip -> Strip
adjustSeparation h strip =
  {strip | loc <- (strip.i * (ceiling <| hexW h), snd strip.loc)}

update : Event -> State -> State
update event s = case event of
  GotoPlay      -> {s | screen <- Play initialGameState}
  Drop          -> drop s
  Resize w h    -> {s | width <- w
                        , height <- h
                        , screen <- case s.screen of
                            Play gs   -> Play <| map (adjustSeparation h) gs
                            otherwise -> s.screen 
                    }
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
                    , "color"     := "white"
                    , "textAlign" := "center"
                    , "fontSize"  := px << tf <| h // 30
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
  in flow outward [ collage hw hh
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
  y >= h - hHEIGHT h n

drawStrip : Int -> Int -> Strip -> Element
drawStrip w h strip = positionedStrip strip.color strip.n (w,h) strip.loc

stage : Int -> Int -> GameState -> Element
stage w h gamestate = layers <| map (drawStrip w h) gamestate 

startScreen : Int -> Int -> Element
startScreen w h = container w h middle [markdown| # Click to start |]

render : (Int, Int) -> State -> Element
render (w,h) state =
  let {screen, width, height} = (watch "state" state)
  in case (screen) of
    Start           ->  startScreen w h
    Play gamestate  ->  stage w h gamestate 
    otherwise       ->  asText "Not defined" 

-- PLUMBING

startClick : Signal Event
startClick = (always GotoPlay)     <~ Mouse.clicks  

dropSignal : Signal Event
dropSignal = (always Drop) <~ fps 30


resizeSignal : Signal Event
resizeSignal = (\(x,y) -> Resize x y) <~ sampleOn (every second) Window.dimensions

playSignal : Signal Event
playSignal = merges [ startClick 
                    , dropSignal
                    , resizeSignal
                    ]

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState playSignal
