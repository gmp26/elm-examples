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

again : GI.Input ()         -- not sure what this button does yet.
again = GI.input ()         -- play again, reset, another strip set?

-- MODEL
grain : Int
grain = 22

size : Int -> Float 
size h = tf h / (1.8 * tf grain)

type Location = (Int, Int) 

type Base   =   {
                
}

type Strip  =   { i : Int
                , color : Color
                , loc : Location
                , n : Int
                , v : (Int, Int)
                , side : Side
                }

data Side = L | R | None


baseStrip   = {v = (0,0), side = None, i = -1,  color = red, loc = (-40,-300),  n = 3}

strip_1     =   {baseStrip | i <- -1,  color <- red, loc <- (-40,-300),  n <- 3}
strip0      =   {baseStrip | i <- 0,  color <- green, loc <- (0,-300),  n <- 5}
strip1      =   {baseStrip | i <- 1,  color <- lightBlue, loc <- (40,-300),  n <- 6}

type GameState  =   { strips    : [Strip]       -- all strips (or undropped maybe?)
                    , dropped   : [Strip]       -- strips currently dropped - in drop order
                    }

data State      =   Start | Play GameState | GameOver


initialGameState : State
initialGameState =  Play {strips = [strip_1, strip0, strip1], dropped = []}

initialState : State
initialState = Start

stackHeight : Side -> GameState -> Int
stackHeight side gs =
    let stack  = filter (\strip -> strip.side == side) gs.strips
    in  foldr (\strip x -> strip.n + x) 0 (stack) 

-- UPDATE

data Event = GotoPlay | Launch Int Side | Drop Int Int | Again

-- update velocity of a clicked strip, leaving the others untouched
launch : Int -> Side -> Strip -> Strip
launch clickedIndex side s  =   let offset = case side of
                                                L -> -100
                                                R -> 100
                                                otherwise -> 0
                                in  {s| v   <-  if clickedIndex == s.i
                                                then (0, 20)
                                                else s.v
                                    ,   side <- if clickedIndex == s.i
                                                then side
                                                else s.side
                                    }

hitBottom : Int -> Int -> Int -> Int -> Bool
hitBottom h y n travelh = 
    y >= travelh - stripHeight h n

moveStrip : Int -> Int -> GameState -> Strip -> Strip
moveStrip w h gs s = 
    let sign =  if  | s.side == L -> -1
                    | s.side == R -> 1
                    | otherwise -> 0
        separation s h = hexW h * sign // 2
        stack = stackHeight s.side gs - s.n
        leftStack = watch "lStack" <| stackHeight L gs
        rightStack = watch "rStack" <| stackHeight R gs

    in if hitBottom h (snd s.loc) s.n (h - stripHeight h stack)
        then    {s| v <- (0, 0)
                ,   loc <- (separation s h,  (h - stripHeight h stack - stripHeight h s.n))
                }
        else    {s| loc <- (separation s h + fst s.v, snd s.loc + snd s.v) }

drop : Int -> Int -> State -> State
drop w h screen =
    case screen of
        Play gs     ->  Play {gs | strips <- map (moveStrip w (h - widthOf againButton) gs) gs.strips}
        otherwise   -> screen

--returns strips at start position. Should be tweened.
resetStrip : Strip -> Strip
resetStrip s =  if  | s.i == -1 -> strip_1
                    | s.i == 0  -> strip0
                    | s.i == 1  -> strip1 
                


update : Event -> State -> State
update event screen = case (unwatch "events" event) of
    GotoPlay    ->  case screen of
                        Play _      -> screen
                        otherwise   -> initialGameState

    Launch index side   
                ->  case screen of
                        Play gs -> Play {gs | strips <- map (launch index side) gs.strips}
                        otherwise   -> screen

    Drop w h    ->  drop w h screen
    Again       ->  case screen of
                        Play gs -> Play {gs | strips <- map resetStrip gs.strips}
                        otherwise   -> screen
                    
    otherwise   ->  initialState

-- DISPLAY

makeButton : Side -> Strip -> Element
makeButton side strip = 
    let col =   if  | strip.i == -1 -> "red"
                    | strip.i == 0  -> "green"
                    | strip.i == 1  -> "blue"
        normal  = GE.image 40 40 <| "media/"++col++".png"
        active  = GE.image 40 40 <| "media/"++col++"Active.png"
    in  GI.customButton startDrop.handle (strip.i, side) normal active active

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
        barGround = collage w barHeight [rect (tf w) (tf barHeight) |> filled black]
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
    let url = "media/"  ++  (if  | col == lightBlue  -> "blue"
                                 | col == red        -> "red"
                                 | col == green      -> "green"
                            ) ++  "Active.png"

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
        Play gs  ->  stage w h gs 
        otherwise       ->  asText "Not defined" 

-- PLUMBING

startClick : Signal Event
startClick = (always GotoPlay) <~ Mouse.clicks  

dropping : Signal Event
dropping = (\(w,h) -> Drop w h) <~ sampleOn (fps 60) Window.dimensions

dropStart : Signal Event
dropStart = (\(i, side) -> Launch i side) <~ startDrop.signal

againSignal : Signal Event
againSignal = (always Again) <~ again.signal 

eventSignal : Signal Event
eventSignal = merges    [ startClick 
                        , dropping
                        , dropStart
                        , againSignal
                        ]

main : Signal Element
main = render <~ Window.dimensions ~ foldp update initialState eventSignal
