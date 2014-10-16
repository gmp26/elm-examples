module Model where

import Utils (tf, gridSize, gsz)
import Vector as V
import Vector (plus, minus)
import DragAndDrop as DD
import Debug (log)


-- n and loc both use integer grid Locations using unit grid squares.
type Strip      =   { n         : Int 
                    , loc       : V.Vector Float
                    , dragging    : Bool
                    }

makeStrip : Int -> V.Vector Float -> Strip
makeStrip n loc = { n = n, loc = loc, dragging = False} 

type GameState  =   { strips : [Strip]
                    }

initialGame : GameState          
initialGame =   { strips =  [ align TL (-1,0) {n = 1, loc = (0,0), dragging = False}
                            , align TL (0,0) {n = 3, loc = (0,0), dragging = False}
                            , align TL (1,0) {n = 9, loc = (0,0), dragging = False}
                            ]
                }
data State = Start | Play GameState

data Event = GotoPlay | Drag (Maybe (Strip, DD.Action))

initialState : State
initialState = Start

color : Strip -> Color 
color strip = case strip.n of
  1 ->  white
  2 ->  lightRed
  3 ->  green
  4 ->  lightPurple
  5 ->  yellow
  6 ->  darkGreen
  7 ->  black
  8 ->  darkBrown
  9 ->  blue
  10 -> orange
  otherwise -> grey

textColor : Strip -> Color 
textColor strip = case strip.n of
  1 ->  black
  2 ->  red
  3 ->  green
  4 ->  lightPurple
  5 ->  yellow
  6 ->  darkGreen
  7 ->  black
  8 ->  darkBrown
  9 ->  blue
  10 -> orange
  otherwise -> grey

type Bounds     = { topLeft     : V.Vector Float
                  , bottomRight : V.Vector Float
                  }

box : Strip -> Bounds
box strip = let u = (-(gsz / 2), (gsz * tf strip.n) / 2)
            in  { topLeft = strip.loc `plus` u
                , bottomRight = strip.loc `minus` u
                }

data Alignment = TL | TR | BL | BR

-- align a strip corner to a grid point
align : Alignment -> V.Vector Int -> Strip -> Strip
align corner at strip =
    let bb = box strip
--        logat = log "alignAt" at
--        fromloc = log "fromLoc" strip.loc
        atF = V.scale gsz (V.toFloat at)
        bbtl = bb.topLeft
--        newloc = log "toloc" <| strip.loc `plus` (atF `minus` bb.topLeft)
    in case corner of
        TL  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.topLeft) }
        TR  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.bottomRight, V.y bb.topLeft)) }
        BL  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.topLeft, V.y bb.bottomRight)) }
        BR  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.bottomRight) }

-- align a strip to the nearest grid point
toGrid : Strip -> Strip
toGrid strip =
    let bb = box strip
        tl = bb.topLeft
        grid coord = (coord / gsz) |> round
    in  align TL (grid <| (V.x tl), grid <| (V.y tl)) strip

setDragging : Bool -> Strip -> Strip
setDragging b s = {s | dragging <- b} 

-- tests

testStrip : Int -> Strip
testStrip n = align BR (n-5,-5) {n = n, loc = (0, 0), dragging = False}

main : Element
main = [1..10]
            |> map (box << testStrip)
            |> asText


