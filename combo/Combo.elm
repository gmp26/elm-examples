module Combo where

import Debug (watch)
import Mouse
import Window
import Random
import Text as T
import Html (Html, toElement, style, prop, text)
import Html.Tags (div)
import Html.Attributes (src, class)
import Graphics.Input as GI

-- HELPERS
unwatch : String -> a -> a      -- use to temporarily silence a watch
unwatch _ a = a

tf : Int -> Float
tf = toFloat

-- INPUTS
startDrop : GI.Input Int
startDrop = GI.input 0     -- strip 0 is the initial value of the signal

-- MODEL
grain : Int
grain = 22

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

type Strip  =   { i : Int
                , color : Color
                , loc : Location
                , n : Int
                , v : (Int, Int)
                }

strip_1     =   {i = -1,  color = red,    loc = (-40,0),  n = 3,  v = (0,0)}
strip0      =   {i = 0,   color = green,  loc = (0,0),    n = 6,  v = (0,0)}
strip1      =   {i = 1,   color = blue,   loc = (40,0),   n = 5,  v = (0,0)}   

type GameState    =   [Strip]
data ScreenState  =   Start | Play GameState | GameOver
type State        =   { screen  : ScreenState
                      , width   : Int
                      , height  : Int
                      }

type Location = (Int, Int) 

initialGameState =  Play
                    [ strip_1
                    , strip0
                    , strip1
                    ]

initialState =  { screen = Start
                , width = 320
                , height = 480
                }

-- UPDATE

data Event = GotoPlay | Launch Int | Drop Int Int

-- update velocity of a clicked strip, leaving the others untouched
launch : Int -> Strip -> Strip
launch clickedIndex s = {s| v   <-  if clickedIndex == s.i
                                        then (0, 3)
                                        else s.v
                        }

moveStrip : Int -> Int -> Strip -> Strip
moveStrip w h s = 
    let separation s h = s.i * (ceiling <| hexW h)
    in if hitBottom h (snd s.loc) s.n
        then    {s| v <- (0, 0)
                ,   loc <- (separation s h, h - s.n*(round <| hexH h))
                }
        else    {s| loc <- (separation s h + fst s.v, snd s.loc + snd s.v) }

drop : Int -> Int -> State -> State
drop w h s =
  let {screen, width, height} = s
  in case screen of
    Play gs     ->  {s| screen  <- Play <| map (moveStrip w h) gs
                    ,   width   <- w
                    ,   height  <- h
                    }
    otherwise   -> s

update : Event -> State -> State
update event s = case (unwatch "events" event) of
    GotoPlay    ->  {s| screen  <-  case s.screen of
                                    Play _      -> s.screen
                                    otherwise   -> initialGameState
                    }
    Launch index -> {s| screen  <-  case s.screen of
                                    Play strips -> Play <| map (launch index) strips
                                    otherwise   -> s.screen
                    }
    Drop w h  ->  drop w h s
    otherwise ->  initialState

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
hexAt col h j   =   ngon 6 (size h)
                    |> filled col
                    |> moveY -(tf j * (hexH h))

px : number -> String
px x = (show x) ++ "px"  

hotSpot :  Int -> Int -> Element
hotSpot h n     = div   [ class "hotspot"
                        , style [ prop "width" (hexW h |> px)
                                , prop "height" (hexH h * tf n |> px)
                                , prop "color" "white"
                                , prop "text-align" "center"
                                , prop "fontSize" ((10 * h) // (12 * grain) |> px)
                                , prop "cursor" "pointer"
                                , prop "pointerEvents" "auto"
                                ]  
                        ]
                        [ show n |> text ]

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
    in flow outward
        [ collage hw hh
            <|  [ hexStripForm h col n |> moveY  ((tf hh - hexH h)/ 2)]
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

clickableStrip : Int -> Int -> Strip -> Element
clickableStrip w h strip = GI.clickable startDrop.handle strip.i (drawStrip w h strip)

stage : Int -> Int -> GameState -> Element
stage w h gamestate = layers <| map (clickableStrip w h) gamestate 

startScreen : Int -> Int -> Element
startScreen w h = container w h middle [markdown| # Click to start |]

render : (Int, Int) -> State -> Element
render (w,h) state =
    let {screen, width, height} = (unwatch "state" state)
    in case (screen) of
        Start           ->  startScreen w h
        Play gamestate  ->  stage w h gamestate 
        otherwise       ->  asText "Not defined" 

-- PLUMBING

startClick : Signal Event
startClick = (always GotoPlay) <~ Mouse.clicks  

dropSignal : Signal Event
dropSignal = (\(w,h) -> Drop w h) <~ sampleOn (fps 30) Window.dimensions

eventSignal : Signal Event
eventSignal = merges    [ startClick 
                        , dropSignal
                        , stripClick
                        ]

stripClick : Signal Event
stripClick = (\i -> Launch i) <~ startDrop.signal

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState eventSignal
