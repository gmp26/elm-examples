module Combo where

import Debug (watch, log)
import Mouse
import Window
import Random
import Text as T
import Maybe (maybe)
import Html (Html, toElement, style, prop, text)
import Html.Tags (div)
import Html.Attributes (src, class)
import Graphics.Input as GI
import Graphics.Element as GE

--
-- HELPERS
--
unwatch : String -> a -> a      -- use to temporarily silence a watch
unwatch _ a = a

tf : Int -> Float
tf = toFloat

--
-- INPUTS
--
startDrop : GI.Input (Strip, Side)
startDrop = GI.input (strip0, None)      -- strip 0 is the initial value of the signal

again : GI.Input ()                 -- undrops
again = GI.input ()         

--
-- MODEL
--
grain : Int
grain = 20

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

speed : Float
speed = 3 * sPerFrame          -- pixels per frame if v == 1

framesPerSec : Float
framesPerSec = 30

sPerFrame : Float
sPerFrame = 1/framesPerSec

type Location = (Int, Int) 

type Color = String

type Strip  =   { color : Color
                , loc : Location
                , n : Int
                , v : Float
                , side : Side
                , dropped : Maybe Int
                }

data Side = L | R | None


baseStrip   = {v = 0, side = None, color = "red"
              , loc = (-40,-300),  n = 3, dropped = Nothing}

-- initial strips
strip_1     =   {baseStrip | color <- "red", loc <- (-40,-300),  n <- 3}
strip0      =   {baseStrip | color <- "green", loc <- (0,-300),  n <- 5}
strip1      =   {baseStrip | color <- "blue", loc <- (40,-300),  n <- 6}

type GameState  =   { strips    : [Strip]
                    , diffTime  : Maybe Time    -- time to do difference drop
                    , reached   : [Int]
                    }

data State      =   Start | Play GameState | GameOver

type Reached = [Int]

initialGameState : State
initialGameState =  Play {strips = [strip_1, strip0, strip1]
                         , diffTime = Nothing
                         , reached = []
                         }

initialState : State
initialState = Start

-- This returns the number of hexes dropped on a side before a given strip.
alreadyDroppedHexes : Side -> Strip -> GameState -> Int
alreadyDroppedHexes side strip gs =
    let droppedBefore s1 s2  = 
            case (s1.dropped, s2.dropped) of
                (Just m, Just n) -> m < n
                otherwise -> False
        stack  = filter (\s -> s.side == side && s `droppedBefore` strip) gs.strips
    in foldr (\aStrip x -> aStrip.n + x) 0 (stack)

dropCount : GameState -> Int
dropCount gs = filter (\strip -> strip.dropped /= Nothing) gs.strips |> length

sideDropCount : Side -> GameState -> Int
sideDropCount side gs = 
    let sideStrips = filter (\strip -> strip.dropped /= Nothing && strip.side == side) gs.strips
    in foldr (\aStrip x -> aStrip.n + x) 0 sideStrips

allDropped : GameState -> Bool
allDropped gs = all (\strip -> strip.dropped /= Nothing) gs.strips

diffCount : GameState -> Int
diffCount gs = min (sideDropCount L gs) (sideDropCount R gs)

--
-- UPDATE
--
data Action = GotoPlay | Launch Strip Side | Animate Int Int Float | Again | Diff

type Event = (Time, Action)

-- update velocity of a clicked strip, leaving the others untouched
launch : GameState -> Strip -> Side -> Strip -> Strip
launch gs clickedStrip side s  =
    if clickedStrip == s
        then    {s| v   <-  20
                , side <- side
                , dropped <- Just <| dropCount gs 
                }
        else    s   -- strip unchanged

-- relaunch a dropped strip
relaunch : GameState -> Strip -> Strip
relaunch gs s = if s.dropped /= Nothing then {s| v <- 5} else s

-- end stop when dropping
hitBottom : Int -> Int -> Int -> Int -> Bool
hitBottom h y n travelh = y >= travelh - stripHeight h n

-- end stop when raising
aboveTop : Int -> Strip -> Bool
aboveTop h strip = (snd strip.loc) + stripHeight h strip.n < 0

moveStrip : Int -> Int -> Float -> GameState -> Strip -> Strip
moveStrip w h delta gs s = 
    let sign =  if  | s.side == L -> -1
                    | s.side == R -> 1
                    | otherwise -> 0
        separation = hexW h * sign // 2
        stack = alreadyDroppedHexes s.side s gs
        dy = s.v * speed * delta |> round
        debugV = watch "v" s.v
        debugS = watch "speed" speed
        debugD = watch "delta" delta
        --debug_gs = unwatch "GameState red" <| head gs.strips

    in if | s.v > 10
            -- dropping
            -> let boxSize = h - stripHeight h stack
               in if hitBottom h (snd s.loc) s.n boxSize
                then    {s| v <- 0
                        ,   loc <- (separation,  (boxSize - stripHeight h s.n))
                        }
                else    {s| loc <- (separation, snd s.loc + dy) }

        | s.v > 0 && s.v < 10
            -- diffing
            -> let diffDrop = diffCount gs
                   boxSize = h - stripHeight h (stack - diffDrop)
               in if hitBottom h (snd s.loc) s.n boxSize
                    then    {s| v <- 0
                            ,   loc <- (separation,  (boxSize - stripHeight h s.n))
                            }
                    else    {s| loc <- (separation, snd s.loc + dy) }

        | s.v < 0
            -- raising
            -> if aboveTop h s
                then    {s| v <- 0
                        ,   loc <- (separation, -(stripHeight h s.n))
                        ,   side <- None
                        ,   dropped <- Nothing
                        }
                else    {s| loc <- (separation, snd s.loc + dy) }

        | otherwise 
            -> {s| loc <- (separation, snd s.loc) }

isMoving  : Strip -> Bool
isMoving strip = strip.v /= 0

animate : Int -> Int -> Float -> State -> State
animate w h delta state =
    case state of
        Play gs ->
            Play    {gs | strips <- map (moveStrip w (h - widthOf againButton) delta gs) gs.strips
                    }
        otherwise   -> state

-- launches a strip upwards
raiseStrip : Strip -> Strip
raiseStrip s = {s | v <- -50}
                
update : Event -> State -> State
update event state = watch "state" <| case event of
    (_, GotoPlay) -> case state of
        Play _      -> state
        otherwise   -> initialGameState

    (t, Launch strip side) ->  case state of
        Play gs ->
            Play {gs | strips <- map (launch gs strip side) gs.strips
                 ,     diffTime <- Just (t + 2*second)
                 }
        otherwise   -> state

    (t, Animate w h delta) -> case state of
        Play gs ->  
            let timeToDiff = (allDropped gs) && maybe False (\dt -> dt < t) gs.diffTime
                -- console = log "(t, dt) = " (t, gs.diffTime)
            in if timeToDiff
                then Play {gs | strips <- map (relaunch gs) gs.strips
                          ,     diffTime <- Nothing
                          }
                else animate w h delta state
        otherwise   -> state

    (_, Again)       ->  case state of
        Play gs -> Play {gs | strips <- map raiseStrip gs.strips
                        ,     diffTime <- Nothing
                        }
        otherwise   -> state
                    
    otherwise   ->  initialState

--
-- DISPLAY
--
makeButton : Side -> Strip -> Element
makeButton side strip = 
    if strip.side == None
        then
            let colorPartUrl = "media/" ++ strip.color
                normal  = GE.image 40 40 <| colorPartUrl ++ ".png"
                active  = GE.image 40 40 <| colorPartUrl ++ "Active.png"
            in GI.customButton startDrop.handle (strip, side) normal active active
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
        padAgainSpacer = spacer 5 10 -- pad around againButton
        pad = (w - ((widthOf againButton) + 2 * (widthOf leftButtons + widthOf padAgainSpacer)) ) // 2
        barHeight = 40
        -- reduce background rect width to prevent horizontal scroll visibility jitter. 
        barGround = collage w barHeight [rect (tf <| w - 2) (tf barHeight) |> filled black |> move (1,0)]
        buttons = flow right 
            [ spacer pad 10
            , leftButtons
            , padAgainSpacer
            , againButton
            , padAgainSpacer
            , rightButtons
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
render (w,h) state =
    case state of
        Start           ->  startScreen w h
        Play gs         ->  stage w h gs 
        otherwise       ->  asText "Not defined" 
--
-- PLUMBING
--
startClick : Signal Action
startClick = (always GotoPlay) <~ Mouse.clicks  

dropStart : Signal Action
dropStart = (\(strip, side) -> Launch strip side) <~ startDrop.signal

againSignal : Signal Action
againSignal = (always Again) <~ again.signal 

eventSignal : Signal Event
eventSignal = merges    [ startClick 
                        , dropStart
                        , againSignal
                        , animationSignal
                        ]
                |> timestamp

-- droppingSignal = (2.2 * second) `since` (merge dropStart againSignal)


animationSignal : Signal Action
animationSignal = (\((w,h), delta) -> Animate w h delta) <~ ((,) <~ Window.dimensions ~ (fps framesPerSec))


main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState eventSignal
