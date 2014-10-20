{--}
module Model    ( Strip, initialState, initialGame
                , color, GameState, State (..), testStrip
                , Event (..), align, Alignment (..)
                , overlaps, box, transparent
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
initialGame =   { strips =  [ align BL (-5,-5) <| makeStrip 2 (0,0)
                            , align BL (-4,-5) <| makeStrip 4 (0,0)
                            , align BL (-3,-5) <| makeStrip 6 (0,0)
                            , align BL (-2,-5) <| makeStrip 10 (0,0)
                            ]
                }
data State = Start | Play GameState

data Event = GotoPlay | Drag (Maybe (Strip, DD.Action))

initialState : State
initialState = Start

transparent : Color -> Color
transparent color = 
    let col = toRgb color
    in rgba col.red col.green col.blue 1.0

color : Strip -> Color 
color strip = if strip.highlight
    then red
    else case strip.n of
        1 -> transparent white 
        2 -> transparent lightRed
        3 -> transparent green
        4 -> transparent lightPurple
        5 -> transparent yellow
        6 -> transparent darkGreen
        7 -> transparent black
        8 -> transparent darkBrown
        9 -> transparent blue
        10 -> transparent orange
        otherwise -> transparent grey

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


overlaps : Strip -> Strip -> Bool
overlaps s1 s2 =
    let b2 = box s2
        b1 = box s1
        epsilon = 1e-6
        result  =  s1.n /= s2.n 
                && (V.x s1.loc) > (V.x b2.topLeft) + epsilon
                && (V.x s1.loc) < (V.x b2.bottomRight) - epsilon
                && (V.y b1.topLeft) > (V.y b2.bottomRight) + epsilon
                && (V.y b1.bottomRight) < (V.y b2.topLeft) - epsilon
        
    in result

-- tests

testStrip : Int -> Strip
testStrip n = align BR (n-5,-5) <| makeStrip n (0,0)

main : Element
main = [1..10]
            |> map (box << testStrip)
            |> asText


