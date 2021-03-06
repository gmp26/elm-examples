{--}
module Model    ( Strip, initialState, initialGame
                , color, backgroundColor
                , GameState, State (..), testStrip
                , Event (..), align, Alignment (..)
                , overlaps, box, transparent
                , aboveOrBelow, TLBR
                ) where
--}
{--
module Model where
--}
import Utils (tf, gridSize, gsz)
import Vector as V
import Vector (plus, minus)
import DragAndDrop as DD
import Tuples as R
import Debug (log, watch)


-- n and loc both use integer grid Locations using unit grid squares.
type Strip      =   { n         : Int
                    , loc       : V.Vector Float
                    , dragging  : Bool
                    , highlight : Bool
                    }

makeStrip : Int -> V.Vector Float -> Strip
makeStrip n loc = { n = n, loc = loc, dragging = False, highlight = False } 

type GameState  =   { strips    : [Strip] -- all strips
                    , reached   : [Int]
                    , leftStack : [Strip]
                    , rightStack : [Strip]
                    , stackBase : V.Vector Float
                    , w         : Int
                    , h         : Int
                    }

initialGame : GameState          
initialGame =   
    let gs =    { strips =  [ align BL (-7,-9) <| makeStrip 1 (0,0)
                            --, align BL (-9,-9) <| makeStrip 2 (0,0)
                            , align BL (-6,-9) <| makeStrip 3 (0,0)
                            --, align BL (-7,-9) <| makeStrip 4 (0,0)
                            --, align BL (-6,-9) <| makeStrip 5 (0,0)
                            --, align BL (-5,-9) <| makeStrip 6 (0,0)
                            --, align BL (-4,-9) <| makeStrip 7 (0,0)
                            --, align BL (-3,-9) <| makeStrip 8 (0,0)
                            , align BL (-5,-9) <| makeStrip 9 (0,0)
                            --, align BL (-1,-9) <| makeStrip 10 (0,0)
                            ]
                , reached = []
                , leftStack = []
                , rightStack = []
                , stackBase = (0,0)
                , w = 100
                , h = 100
                }
    in {gs | reached <- map (\s -> s.n) gs.strips} 


data State = Start | Play GameState

data Event = GotoPlay | Drag (Maybe (Strip, DD.Action)) | Resize (Int,Int)

initialState : State
initialState = Start

changeStrips : [Strip] -> GameState -> GameState
changeStrips strips gs =
    { gs | strips <- strips
    , reached <- reachables gs
    }

reachables : GameState -> [Int]
reachables gs = 
    map (\s -> s.n) gs.strips
        |> R.reachables

transparent : Color -> Color
transparent color = 
    let col = toRgb color
    in rgba col.red col.green col.blue 1.0

backgroundColor : Color
backgroundColor = charcoal

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

type TLBR = { topLeft     : V.Vector Float
            , bottomRight    : V.Vector Float
            }

type AllBounds a =  { a |   bottomLeft  : V.Vector Float
                    ,       topRight : V.Vector Float
                    }

type Bounds   = AllBounds TLBR

box : Strip -> Bounds
box strip = let u = (-(gsz / 2), (gsz * tf strip.n) / 2)
                tl = strip.loc `plus` u
                br = strip.loc `minus` u
                tr = (V.x br, V.y bl)
                bl = (V.x br, V.y br)

            in  { topLeft = tl
                , topRight = tr
                , bottomLeft = bl
                , bottomRight = br
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


aboveOrBelow  : Strip -> Strip -> Bool
aboveOrBelow s1 s2 =
    let b2 = box s2
        b1 = box s1
        epsilon = 1e-6  
        result  =  V.abs (b2.topLeft `V.minus` b1.bottomLeft) < epsilon
                || V.abs (b2.bottomLeft `V.minus` b1.topLeft) < epsilon
    in result



-- tests

testStrip : Int -> Strip
testStrip n = align BR (n-5,-5) <| makeStrip n (0,0)

main : Element
main = [1..10]
            |> map (box << testStrip)
            |> asText


