module Model where

import Utils (tf)
import Vector as V
import Vector (plus, minus)
import DragAndDrop as DD

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
initialGame =   { strips = []
                }
data State = Start | Play GameState

data Event = GotoPlay | Drag (Maybe (Strip, DD.Action))

initialState : State
initialState = Start

color : Strip -> Color 
color strip = case strip.n of
  1 ->  white
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
box strip = let u = V.x strip.loc
                v = V.y strip.loc + (tf strip.n) / 2
            in  { topLeft = (-u, v)
                , bottomRight = (u+1, -v)
                }

data Alignment = TL | TR | BL | BR

-- align a strip corner to a grid point
align : Alignment -> V.Vector Int -> Strip -> Strip
align corner at strip =
    let bb = box strip
        atF = V.toFloat at
    in case corner of
        TL  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.topLeft) }
        TR  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.bottomRight, V.y bb.topLeft)) }
        BL  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.topLeft, V.y bb.bottomRight)) }
        BR  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.bottomRight) }


-- tests

testStrip : Int -> Strip
testStrip n = align BL (n-5,-5) {n = n, loc = (0, 0), dragging = False}

testDraw : GameState
testDraw    =   { strips = [1..10] |> map testStrip
                }

main : Element
main = [1..10]
            |> map (box << testStrip)
            |> asText


