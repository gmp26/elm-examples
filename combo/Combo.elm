module Combo where

import Mouse
import Window
import Random
import Debug

-- CONFIG

-- HELPER FUNCTIONS

baseUnit : Int
baseUnit = 20

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

hex : Color -> Float -> Form
hex col size = ngon 6 size
               |> filled col  

nhex : Float -> Int -> Form
nhex size rank = 
  let h = size * (tf rank)
  in moveY h <| hex red size

strip : Int -> Shape
strip n = rect (tf baseUnit) (tf <| n * baseUnit)

colorStrip : Color -> Int -> (Float, Float) -> [Form]
colorStrip col n pos = [move pos <| filled col <| strip n]

rod col n pos = 
  let w = baseUnit
      h = baseUnit * n
  in collage w h <| colorStrip col n pos

startScreen : Element
startScreen = [markdown| 
# Click to start
|]

render : (Int, Int) -> State -> Element
render (w,h) screen = case screen of
  Start -> startScreen
  Play ->   container w h midTop <| rod black 3 (0,0) 
  otherwise -> asText "Not defined" 

-- PLUMBING

startClick = (always GotoPlay)     <~ Mouse.clicks  

main = render <~ Window.dimensions ~ foldp update initialState startClick
--main = asText <~ (relativeMouse <~ (center <~ Window.dimensions) ~ Mouse.position)
