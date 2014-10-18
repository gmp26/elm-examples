{--}
module Model    ( Strip, initialState, initialGame
                , color, GameState, State (..), testStrip
                , Event (..), align, Alignment (..)
                , overlaps, box
                ) where
--}
{--
module Model where
--}
import Utils (tf, gridSize, gsz)
import Vector as V
import Vector (plus, minus)
import DragAndDrop as DD
import Debug (log, watch)


-- n and loc both use integer grid Locations using unit grid squares.
type Strip      =   { n         : Int
                    , loc       : V.Vector Float
                    , dragging  : Bool
                    , highlight : Bool  
                    }

makeStrip : Int -> V.Vector Float -> Strip
makeStrip n loc = { n = n, loc = loc, dragging = False, highlight=False} 

type GameState  =   { strips : [Strip]
                    }

initialGame : GameState          
initialGame =   { strips =  [ align BL (-5,-5) <| makeStrip 1 (0,0)
                            , align BL (-4,-5) <| makeStrip 3 (0,0)
                            , align BL (-3,-5) <| makeStrip 9 (0,0)
                            ]
                }
data State = Start | Play GameState

data Event = GotoPlay | Drag (Maybe (Strip, DD.Action))

initialState : State
initialState = Start

color : Strip -> Color 
color strip = if strip.highlight
    then red
    else case strip.n of
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
        atF = V.scale gsz (V.toFloat at)
        bbtl = bb.topLeft
    in case corner of
        TL  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.topLeft) }
        TR  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.bottomRight, V.y bb.topLeft)) }
        BL  -> {strip | loc <- strip.loc `plus` (atF `minus` (V.x bb.topLeft, V.y bb.bottomRight)) }
        BR  -> {strip | loc <- strip.loc `plus` (atF `minus` bb.bottomRight) }

-- align a strip to the nearest grid point

overlaps : Strip -> Strip -> Bool
overlaps s1 s2 =
    let b2 = box s1
        b1 = box s2
        result =  (   (V.x b1.bottomRight) > (V.x b2.topLeft)
        &&  (V.x b1.topLeft) < (V.x b2.bottomRight)
        &&  (V.y b1.topLeft) > (V.y b2.bottomRight)
        &&  (V.y b1.bottomRight) < (V.y b2.topLeft)
        )
    in result

-- tests

testStrip : Int -> Strip
testStrip n = align BR (n-5,-5) <| makeStrip n (0,0)

main : Element
main = [1..10]
            |> map (box << testStrip)
            |> asText


