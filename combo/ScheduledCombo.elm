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
import Scheduler as Tasks

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
startDrop = GI.input (strip0, L) -- (strip0, L) is the initial value of the signal

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
speed = 6 * sPerFrame   -- 6 is good, 1 in debug

framesPerSec : Float
framesPerSec = 60

sPerFrame : Float
sPerFrame = 1/framesPerSec

type Location = (Int, Int) 

type Color = String

type Strip  =   { color     : Color             -- also use to identify a strip
                , loc       : Location
                , n         : Int
                , v         : Float
                , side      : Side
                , dropOrder : Maybe Int
                , animation : Animation
                }


-- Whese can apply to an individual Strip and also, independently, to the GameState
data Animation  = Dropping | Dropped |
                  Raising | Raised |
                  Diffing | Diffed |
                  UnDiffing | UnDiffed

data Side = L | R

baseStrip   = {v = 0, side = L, color = "red"
              , loc = (-40,-300),  n = 3, dropOrder = Nothing, animation = Raised}

-- initial strips
strip_1     =   {baseStrip | color <- "red", loc <- (-40,-300),  n <- 3}
strip0      =   {baseStrip | color <- "green", loc <- (0,-300),  n <- 5}
strip1      =   {baseStrip | color <- "blue", loc <- (40,-300),  n <- 6}

type GameState  =   { strips        : [Strip]               -- all strips
                    , tasks         : [Tasks.Task Strip]    -- scheduled tasks
                    , animation     : Animation
                    , reached       : [Int]         -- the numbers we reached
                    }

data State      =   Start | Play GameState | GameOver

type Reached = [Int]

initialGameState : State
initialGameState =  Play { strips        = [strip_1, strip0, strip1]
                         , tasks        = []
                         , animation    = Raised
                         , reached      = []
                         }

initialState : State
initialState = Start

velocity : Animation -> Float
velocity animation = case animation of
    Dropping   -> 30
    Diffing    -> 30
    UnDiffing  -> -30
    Raising    -> -50
    otherwise  -> 0

finished : Animation -> Bool
finished animation = case animation of
    Dropping   -> False
    Diffing    -> False
    UnDiffing  -> False
    Raising    -> False
    otherwise  -> True

isMoving : Animation -> Bool
isMoving animation = not (finished animation)


-- This returns the number of hexes dropped on a side before a given strip.
droppedOnSideBefore : Side -> Strip -> GameState -> Int
droppedOnSideBefore side strip gs =
    let droppedBefore s1 s2  = 
            case (s1.dropOrder, s2.dropOrder) of
                (Just m, Just n) -> m < n
                otherwise -> False
        stack  = filter (\s -> s.side == side && s `droppedBefore` strip) gs.strips
    in foldr (\s acc -> s.n + acc) 0 (stack)

dropCount : GameState -> Int
dropCount gs = foldr (\strip acc -> if strip.dropOrder /= Nothing  then acc+1 else acc) 0
                    gs.strips 

sideDropCount : Side -> GameState -> Int
sideDropCount side gs = 
    let sideStrips = filter (\strip -> strip.animation == Dropped && strip.side == side) gs.strips
    in foldr (\s acc -> s.n + acc) 0 sideStrips

anyOnSide : GameState -> Side -> Bool 
anyOnSide gs side = any (\s -> s.side == side && s.animation==Dropped) gs.strips

diffable : GameState -> Bool
diffable gs = (anyOnSide gs L) && (anyOnSide gs R)

--unDiffable : GameState -> Bool
--unDiffable gs = any (\s -> s.animation == Diffed) gs.strips

diffCount : GameState -> Int
diffCount gs = min (sideDropCount L gs) (sideDropCount R gs)

waitingForAgain : GameState -> Bool
waitingForAgain gs = 
    isEmpty gs.tasks && all (\s -> isDropped s || isUnDiffed s) gs.strips 

waiting : GameState -> Bool
waiting gs = isEmpty gs.tasks && all (\s -> finished s.animation) gs.strips 

isDropped : Strip -> Bool
isDropped s = s.animation == Dropped

isDiffed : Strip -> Bool
isDiffed s = s.animation == Diffed

isUnDiffed : Strip -> Bool
isUnDiffed s = s.animation == UnDiffed

isRaised : Strip -> Bool
isRaised s = s.animation == Raised

--dropped : [Strip] -> [Strip]
--dropped strips = filter isDropped strips

--allDropped : GameState -> Bool
--allDropped gs = all isDropped gs.strips

--anyDropped : GameState -> Bool
--anyDropped gs = any isDropped gs.strips


--
-- UPDATE
--
data Action = GotoPlay | Drop Strip Side | Animate Int Int Float | Again | Diff

type Event = (Time, Action)

-- update velocity of a clicked strip, leaving the others untouched
drop : GameState -> Strip -> Side -> Strip -> Strip
drop gs clickedStrip side s  =
    if clickedStrip == s
        then    {s| v   <-  velocity Dropping
                ,   animation <- Dropping
                ,   side <- side
                ,   dropOrder <- Just <| dropCount gs 
                }
        else    s   -- strip unchanged

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
        stack = droppedOnSideBefore s.side s gs
        dy = (velocity s.animation) * speed * delta |> round
        -- debugAnim = watch "animation" s.animation
        -- debug_gs = unwatch "GameState red" <| head gs.strips
        -- a = log "animation" (show s.animation)
        -- logD = log "dropped" <| show (s.side, stack)

    in case s.animation of
        Dropping
            -> let boxSize = h - stripHeight h stack
               in if hitBottom h (snd s.loc) s.n boxSize
                    then    {s| animation <- Dropped
                            ,   loc <- (separation,  (boxSize - stripHeight h s.n))
                            }
                    else    {s| loc <- (separation, snd s.loc + dy) }

        Diffing
            -> let diffDrop = diffCount gs
                   boxSize = h - stripHeight h (stack - diffDrop)
               in if hitBottom h (snd s.loc) s.n boxSize
                    then    {s| animation <- Diffed
                            ,   loc <- (separation,  (boxSize - stripHeight h s.n))
                            }
                    else    {s| loc <- (separation, snd s.loc + dy) }

        UnDiffing
            -> let boxSize = h - stripHeight h stack
               in if not <| hitBottom h (snd s.loc) s.n boxSize
                    then    {s| animation <- UnDiffed
                            ,   loc <- (separation,  (boxSize - stripHeight h s.n))
                            }
                    else    {s| loc <- (separation, snd s.loc + dy) }

        Raising
            -> if aboveTop h s
                then    {s| animation <- Raised
                        ,   loc <- (separation, -(stripHeight h s.n))
                        ,   side <- L           -- don't care
                        ,   dropOrder <- Nothing
                        }
                else    {s| loc <- (separation, snd s.loc + dy) }

        otherwise
            ->  s

animateStrip : Int -> Int -> Float -> GameState -> Float -> Strip -> Strip
animateStrip w h delta gs t strip =
    moveStrip w (h - (widthOf <| againButton gs)) delta gs strip

--animate : Int -> Int -> Float -> GameState -> GameState
--animate w h delta gs =
--    {gs | strips <- map (moveStrip w (h - (widthOf <| againButton gs)) delta gs) gs.strips}
 
start : Animation -> (Time -> Strip -> Strip)
start anim = (\_ strip -> {strip | animation <- anim, v <- velocity anim})

raiseStrip : Strip -> Strip
raiseStrip s = {s | animation <- Raising}

update : Event -> State -> State
update event state = case state of
    Start   ->  case event of
                    (_, GotoPlay)   -> initialGameState
                    otherwise       ->  state

    Play gs ->  case event of
        (t, Drop strip side)
            ->  if strip.animation == Raised && waiting gs 
                    then Play {gs | strips <- map (drop gs strip side) gs.strips}
                    else state

{--                        
            ->  Play {gs | strips <- map (drop gs strip side) gs.strips
                     ,     tasks  <- if (dropCount gs > 0)

                                        then (scheduleManyAt (Just Diffing) (t+1*second)
                                            (strip :: dropped gs.strips) gs.tasks) ++
                                            (scheduleManyAt (Just UnDiffing) (t+2*second)
                                            (strip :: dropped gs.strips) gs.tasks)
                                        else gs.tasks
                     }
--}

        (_, Again)
            ->  if waitingForAgain gs
                    then Play {gs | strips <- map raiseStrip gs.strips}
                    else state


        (t, Animate w h delta) ->

                let tasks = if isEmpty gs.tasks && diffable gs
                    then gs.tasks 
                            |> Tasks.schedule (Tasks.make
                                (t+3*second)
                                (start Diffing)
                                isDropped
                                ("-> Diffing @" ++ (show <| t+3*second)))
                            |> Tasks.schedule (Tasks.make
                                    (t+6*second)
                                    (start UnDiffing)
                                    isDiffed
                                    ("-> unDiffing @" ++ (show <| t+6*second)))
                    else gs.tasks

                    tasks' = tasks |> Tasks.schedule (Tasks.make
                                        t
                                        (animateStrip w h delta gs)
                                        (\s -> isMoving s.animation)
                                        "move")

                    (strips'', tasks'') = Tasks.perform t (gs.strips, tasks')      

                in Play {gs | strips <- strips'', tasks <- tasks''}

{--
            ->  let newgs = nextAnimation t gs
                in Play <| animate w h delta newgs
--}

        otherwise
            -> Play gs

    otherwise -> state

--
-- DISPLAY
--
makeButton : GameState -> Side -> Strip -> Element
makeButton gs side strip =
    if strip.animation == Raised && waiting gs 
        then
            let colorPartUrl = "media/" ++ strip.color
                normal  = GE.image 40 40 <| colorPartUrl ++ ".png"
                active  = GE.image 40 40 <| colorPartUrl ++ "Active.png"
            in GI.customButton startDrop.handle (strip, side) normal active active
        else
            GE.image 40 40 <| "media/inActive.png" 

againButton : GameState -> Element
againButton gs = 
    if waitingForAgain gs
        then GI.customButton again.handle ()
                (GE.image 40 40 "media/up.png")
                (GE.image 40 40 "media/upActive.png")
                (GE.image 40 40 "media/upActive.png")
        else GE.image 40 40 <| "media/inActive.png" 

buttonBar : Int -> GameState -> Element
buttonBar w gs = 
    let dropButtons side = map (makeButton gs side) gs.strips
        leftButtons = flow right <| dropButtons L
        rightButtons = flow left <| dropButtons R
        padAgainSpacer = spacer 5 10 -- pad around againButton
        againButton' = againButton gs
        pad = (w - ((widthOf againButton') + 2 * (widthOf leftButtons + widthOf padAgainSpacer)) ) // 2
        barHeight = 40
        -- reduce background rect width to prevent horizontal scroll visibility jitter. 
        barGround = collage w barHeight [rect (tf <| w - 2) (tf barHeight) |> filled black |> move (1,0)]
        buttons = flow right 
            [ spacer pad 10
            , leftButtons
            , padAgainSpacer
            , againButton'
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
dropStart = (\(strip, side) -> Drop strip side) <~ startDrop.signal

againSignal : Signal Action
againSignal = (always Again) <~ again.signal 

eventSignal : Signal Event
eventSignal = merges    [ startClick 
                        , dropStart
                        , againSignal
                        , animationSignal
                        ]
                |> timestamp

-- use this to quash fps so we don't hog the cpu
activitySignal : Signal Bool
activitySignal = (12 * second) `since` Mouse.clicks

animationSignal : Signal Action
animationSignal = (\((w,h), delta) -> Animate w h delta) <~ ((,) <~ Window.dimensions ~ (fpsWhen framesPerSec activitySignal))

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState eventSignal