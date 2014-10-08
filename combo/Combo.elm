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
import Graphics.Element as GE

-- HELPERS
unwatch : String -> a -> a      -- use to temporarily silence a watch
unwatch _ a = a

tf : Int -> Float
tf = toFloat

-- INPUTS
startDrop : GI.Input (Int, Side)
startDrop = GI.input (0, None)      -- strip 0 is the initial value of the signal

again : GI.Input ()                 -- undrops
again = GI.input ()         

-- MODEL
grain : Int
grain = 27

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

type Location = (Int, Int) 

type Color = String

type Strip  =   { i : Int
                , color : Color
                , loc : Location
                , n : Int
                , v : (Int, Int)
                , side : Side
                , dropKey : Int
                , diffed : Bool
                }

data Side = L | R | None


baseStrip   = {v = (0,0), side = None, i = -1,  color = "red"
              , loc = (-40,-300),  n = 3, dropKey = 10, diffed = False}

-- initial strips
strip_1     =   {baseStrip | i <- -1,  color <- "red", loc <- (-40,-300),  n <- 3}
strip0      =   {baseStrip | i <- 0,  color <- "green", loc <- (0,-300),  n <- 5}
strip1      =   {baseStrip | i <- 1,  color <- "blue", loc <- (40,-300),  n <- 6}

type GameState  =   { strips    : [Strip]       -- all strips (or undropped maybe?)
                    }

data State      =   Start | Play GameState | GameOver


initialGameState : State
initialGameState =  Play {strips = [strip_1, strip0, strip1]}

initialState : State
initialState = Start

-- This returns the number of hexes dropped on each side.
stackHexes : Side -> GameState -> Int
stackHexes side gs =
    let stack  = filter (\strip -> strip.side == side) gs.strips
    in  foldr (\strip x -> strip.n + x) 0 (stack) 

-- This returns the number of hexes dropped on a side before a given strip.
-- i.e. the number the given strip should sit on.
alreadyDroppedHexes : Side -> Strip -> GameState -> Int
alreadyDroppedHexes side strip gs =
    let stack  = filter (\s -> s.side == side && s.dropKey < strip.dropKey) gs.strips
    in  foldr (\aStrip x -> aStrip.n + x) 0 (stack) 

-- UPDATE

data Event = GotoPlay | Launch Int Side | Animate Int Int | Again | Diff

-- update velocity of a clicked strip, leaving the others untouched
launch : GameState -> Int -> Side -> Strip -> Strip
launch gs clickedIndex side s  =   
    if clickedIndex == s.i && s.side == None
        then    {s| v   <-  (0, 20)
                , side <- side
                , dropKey <- alreadyDroppedHexes side s gs 
                }
        else    s   -- strip unchanged

-- relaunch a clicked strip
relaunch : GameState -> Strip -> Strip
relaunch gs s  =   
    if s.dropKey < 10
        then    {s| v <-  (0, 5), diffed <- True}
        else    s   -- strip unchanged


-- end stop when dropping
hitBottom : Int -> Int -> Int -> Int -> Bool
hitBottom h y n travelh = 
    y >= travelh - stripHeight h n

-- end stop when raising
aboveTop : Int -> Strip -> Bool
aboveTop h strip = (snd strip.loc) + stripHeight h strip.n < 0

moveStrip : Int -> Int -> GameState -> Strip -> Strip
moveStrip w h gs s = 
    let sign =  if  | s.side == L -> -1
                    | s.side == R -> 1
                    | otherwise -> 0
        separation = hexW h * sign // 2
        stack = alreadyDroppedHexes s.side s gs
        allDiffed = all (\s -> s.diffed) gs.strips
        debug_gs = watch "GameState" gs

    in if | snd s.v > 10
            -- dropping
            -> if hitBottom h (snd s.loc) s.n (h - stripHeight h stack)
                then    {s| v <- (0, 0)
                        ,   loc <- (separation,  (h - stripHeight h stack - stripHeight h s.n))
                        }
                else    {s| loc <- (separation + fst s.v, snd s.loc + snd s.v) }

        | allDiffed && (snd s.v > 0) && (snd s.v < 10)
            -- diffing
            -> if hitBottom h (snd s.loc) s.n (h - stripHeight h (stack - 1))
                then    {s| v <- (0, 0)
                        ,   loc <- (separation,  (h - stripHeight h (stack - 1) - stripHeight h s.n))
                        ,   diffed <- False
                        }
                else    {s| loc <- (separation + fst s.v, snd s.loc + snd s.v) }

        | (snd s.v < 0)
            -- raising
            -> if aboveTop h s
                then    {s| v <- (0,0)
                        ,   loc <- (separation, -(stripHeight h s.n))
                        ,   side <- None
                        ,   dropKey <- 10
                        }
                else    {s| loc <- (separation + fst s.v, snd s.loc + snd s.v) }

        | otherwise 
            -> {s| loc <- (separation + fst s.v, snd s.loc) }

isMoving  : Strip -> Bool
isMoving strip = strip.v /= (0,0)

animate : Int -> Int -> State -> State
animate w h screen =
    case screen of
        Play gs ->
            Play    {gs | strips <- map (moveStrip w (h - widthOf againButton) gs) gs.strips
                    }
        otherwise   -> screen

-- launches a strip upwards
raiseStrip : Strip -> Strip
raiseStrip s = {s | v <- (0, -50)}
                
update : Event -> State -> State
update event screen = case event of
    GotoPlay    ->  case screen of
                        Play _      -> screen
                        otherwise   -> initialGameState

    Launch index side   
                ->  case screen of
                        Play gs -> Play {gs | strips <- map (launch gs index side) gs.strips
                                        }
                        otherwise   -> screen

    Animate w h ->  animate w h screen

    Diff        ->  case screen of
                        Play gs -> Play {gs | strips <- map (relaunch gs) gs.strips }

    Again       ->  case screen of
                        Play gs -> Play {gs | strips <- map raiseStrip gs.strips
                                        }
                        otherwise   -> screen
                    
    otherwise   ->  initialState

-- DISPLAY

makeButton : Side -> Strip -> Element
makeButton side strip = 
    if strip.side == None
        then
            let colorPartUrl = "media/" ++ strip.color
                normal  = GE.image 40 40 <| colorPartUrl ++ ".png"
                active  = GE.image 40 40 <| colorPartUrl ++ "Active.png"
            in GI.customButton startDrop.handle (strip.i, side) normal active active
        else
            -- this strip is already dropped 
            GE.image 40 40 <| "media/inActive.png" 

againButton = GI.customButton again.handle ()
                (GE.image 40 40 "media/up.png")
                (GE.image 40 40 "media/upActive.png")
                (GE.image 40 40 "media/upActive.png")

buttonBar : Int -> GameState -> Element
buttonBar w gs = 
    let launchButtons side = map (makeButton side) gs.strips
        leftButtons = flow right <| launchButtons L
        rightButtons = flow left <| launchButtons R
        pad = (w - ((widthOf againButton) + 2 * (widthOf leftButtons)) ) // 2
        barHeight = 40
        -- reduce background rect width to prevent horizontal scroll visibility jitter. 
        barGround = collage w barHeight [rect (tf <| w - 2) (tf barHeight) |> filled black |> move (1,0)]
        buttons = flow right 
            [ spacer pad 10
            , leftButtons
            , spacer 5 10
            , againButton
            , spacer 5 10
            , rightButtons
            , spacer pad 10 
            ]
    in  flow outward
        [ barGround
        , flow down 
            [ spacer 1 1
            , buttons
            ]
        ]

-- height of hexagon
hexH : Int -> Int
hexH h = round <| (size h) * 1.8

hexW : Int -> Int
hexW h = round <| (size h) * 2

-- a CSS length property in pixels
px : number -> String
px x = (show x) ++ "px"  

-- make a numeric label for a hexStrip in white.
-- h is needed to get the size right.
stripLabel :  Int -> Int -> Element
stripLabel h n = 
    div  
        [ class "hotspot"
        , style 
            [ prop "width" (hexH h |> px)
            , prop "color" "white"
            , prop "text-align" "center"
            , prop "fontSize" ((10 * h) // (12 * grain) |> px)
            ]  
        ]
        [ show n |> text ]

        |> toElement (hexW h) (hexH h)

stripHeight : Int -> Int -> Int
stripHeight h n = n * hexH h

hexStrip : Int -> Color -> Int -> Element
hexStrip h col n = 
    let url = "media/"  ++  col ++  "Active.png"

    in flow down <| map (\i -> fittedImage h h url) [1..n]

labelledStrip : Int -> Color -> Int -> Element
labelledStrip h col n = 
    flow outward
        [ hexStrip (hexH h) col n
        , stripLabel h n
        ]

positionedStrip : Color -> Int -> (Int, Int) -> Location -> Element
positionedStrip col n (w, h) (x,y) =
    let 
        xpos = absolute (x + w // 2)
        ypos = absolute y
        position = midTopAt xpos ypos
    in labelledStrip h col n
        |> container w h position

drawStrip : Int -> Int -> Strip -> Element
drawStrip w h strip = positionedStrip strip.color strip.n (w,h) strip.loc

stage : Int -> Int -> GameState -> Element
stage w h gs = 
    let bar = buttonBar w gs
    in flow outward 
        [ collage w h [rect (tf w) (tf h) |> filled (rgb 50 40 40)]
        , flow down 
            [ bar
            , flow inward <| map (drawStrip w (h - heightOf bar)) gs.strips
            ]
        ] 

startScreen : Int -> Int -> Element
startScreen w h = container w h middle [markdown| # Click to start |]

render : (Int, Int) -> State -> Element
render (w,h) screen =
    case screen of
        Start           ->  startScreen w h
        Play gs         ->  stage w h gs 
        otherwise       ->  asText "Not defined" 

-- PLUMBING
startClick : Signal Event
startClick = (always GotoPlay) <~ Mouse.clicks  

dropStart : Signal Event
dropStart = (\(i, side) -> Launch i side) <~ startDrop.signal

againSignal : Signal Event
againSignal = (always Again) <~ again.signal 

eventSignal : Signal Event
eventSignal = merges    [ startClick 
                        , dropStart
                        , againSignal
                        , diffSignal
                        , animationSignal
                        ]

droppingSignal = (2.2 * second) `since` (merge dropStart againSignal)
diffSignal     = (always Diff) <~ delay (3 * second) dropStart 

animationSignal : Signal Event
-- animationSignal = (\(w,h) -> Animate w h) <~ sampleOn (fpsWhen 60 droppingSignal) Window.dimensions
animationSignal = (\(w,h) -> Animate w h) <~ sampleOn (fps 60) Window.dimensions


main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState eventSignal
