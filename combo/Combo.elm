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

type Strip  =   { i : Int
                , color : Color
                , loc : Location
                , n : Int
                , v : (Int, Int)
                , side : Side
                }

-- should a strip drop on left or right?
data Side = L | R | None

strip_1     =   {i = -1,  color = red,    loc = (-40,-300),  n = 3,  v = (0,0), side = None}
strip0      =   {i = 0,   color = green,  loc = (0,-300),    n = 5,  v = (0,0), side = None}
strip1      =   {i = 1,   color = lightBlue,   loc = (40,-300),   n = 6,  v = (0,0), side = None}   

type GameState    =   [Strip]
data ScreenState  =   Start | Play GameState | GameOver
type State        =   { screen  : ScreenState
                      , width   : Int
                      , height  : Int
                      }

type Location = (Int, Int) 

initialGameState =  Play
                    [ strip_1
                    , strip0
                    , strip1
                    ]

initialState =  { screen = Start
                , width = 320
                , height = 480
                }

stackHeight : Side -> GameState -> Int
stackHeight side strips =
    let stack  = filter (\strip -> strip.side == side) strips
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
                                                then (0, 1)
                                                else s.v
                                    ,   side <- if clickedIndex == s.i
                                                then side
                                                else s.side
                                    }

-- h - s.n*(round <| hexH h)
-- stackHeight s.side strips


hitBottom : Int -> Int -> Int -> Int -> Bool
hitBottom h y n travelh = 
    y >= travelh - hHEIGHT h n

moveStrip : Int -> Int -> GameState -> Strip -> Strip
moveStrip w h strips s = 
    let sign =  if  | s.side == L -> -1
                    | s.side == R -> 1
                    | otherwise -> 0
        separation s h = round <| hexW h * sign / 2
        stack = stackHeight s.side strips - s.n
        leftStack = watch "lStack" <| stackHeight L strips
        rightStack = watch "rStack" <| stackHeight R strips

    in if hitBottom h (snd s.loc) s.n (h - hHEIGHT h stack)
        then    {s| v <- (0, 0)
                ,   loc <- (separation s h,  (h - hHEIGHT h stack - hHEIGHT h s.n))
                }
        else    {s| loc <- (separation s h + fst s.v, snd s.loc + snd s.v) }

drop : Int -> Int -> State -> State
drop w h s =
    let {screen, width, height} = s
    in case screen of
        Play gs     ->  {s| screen  <- Play <| map (moveStrip w (h - widthOf againButton) gs) gs
                        ,   width   <- w
                        ,   height  <- h
                        }
        otherwise   -> s

--returns strips at start position. Should be tweened.
resetStrip : Strip -> Strip
resetStrip s =  if  | s.i == -1 -> strip_1
                    | s.i == 0  -> strip0
                    | s.i == 1  -> strip1 
                


update : Event -> State -> State
update event s = case (unwatch "events" event) of
    GotoPlay    ->  {s| screen  <-  case s.screen of
                                    Play _      -> s.screen
                                    otherwise   -> initialGameState
                    }
    Launch index side   
                ->  {s| screen  <-  case s.screen of
                                        Play strips -> Play <| map (launch index side) strips
                                        otherwise   -> s.screen
                    }
    Drop w h    ->  drop w h s
    Again       ->  {s| screen  <- case s.screen of
                                        Play strips -> Play <| map resetStrip strips
                                        otherwise   -> s.screen
                    }
    otherwise   ->  initialState

-- DISPLAY

-- one coloured hexagon
hex : Color -> Int -> Form
hex col h = ngon 6 (size h)
               |> filled col  

-- height of hexagon
hexH : Int -> Float
hexH h = (size h) * 1.8

hexW : Int -> Float
hexW h = (size h) * 2

-- the jth coloured hexagon in a strip
hexAt : Color -> Int -> Int -> Form
hexAt col h j   =   ngon 6 (size h)
                    |> filled col
                    |> moveY -(tf j * (hexH h))

-- a CSS length property in pixels
px : number -> String
px x = (show x) ++ "px"  

-- make a numeric label for a hexStrip in white.
-- h is needed to get the size right.
stripLabel :  Int -> Int -> Element
stripLabel h n     = div   [ class "hotspot"
                        , style [ prop "width" (hexW h |> px)
                                , prop "color" "white"
                                , prop "text-align" "center"
                                , prop "fontSize" ((10 * h) // (12 * grain) |> px)
                                ]  
                        ]
                        [ show n |> text ]

                  |> toElement (round <| (hexW h)) (round <| (hexH h))

hexStripForm : Int -> Color -> Int -> Form
hexStripForm h col n =
  let hexunit = hexAt col h
  in map hexunit [0..(n-1)]
        |> group

hHEIGHT : Int -> Int -> Int
hHEIGHT h n = (round <| (hexH h))*n

hHeight : Int -> Int -> Float
hHeight h n = (hexH h)*(tf n)

hexStripImage : Int -> Color -> Int -> Element
hexStripImage = image h h 

hexStripElement : Int -> Color -> Int -> Element
hexStripElement h col n = 
    let hh = (round <| (hexH h))*n
        hw = (round <| (hexW h))
    in flow outward
        [ collage hw hh
            <|  [ hexStripForm h col n |> moveY  ((tf hh - hexH h)/ 2)]
        , stripLabel h n
        ]

positionedStrip : Color -> Int -> (Int, Int) -> Location -> Element
positionedStrip col n (w, h) (x,y) =
    let 
        xpos = absolute (x + w // 2)
        ypos = absolute y
        position = midTopAt xpos ypos
    in hexStripElement h col n
        |> container w h position

drawStrip : Int -> Int -> Strip -> Element
drawStrip w h strip = positionedStrip strip.color strip.n (w,h) strip.loc

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
buttonBar w gameState = 
    let launchButtons side = map (makeButton side) gameState
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

stage : Int -> Int -> GameState -> Element
stage w h gameState = 
    let bar = buttonBar w gameState
    in flow outward 
        [ collage w h [rect (tf w) (tf h) |> filled (rgb 50 40 40)]
        , flow down 
            [ bar
            , flow inward <| map (drawStrip w (h - heightOf bar)) gameState
            ]
        ] 

startScreen : Int -> Int -> Element
startScreen w h = container w h middle [markdown| # Click to start |]

render : (Int, Int) -> State -> Element
render (w,h) state =
    let {screen, width, height} = (unwatch "state" state)
    in case (screen) of
        Start           ->  startScreen w h
        Play gamestate  ->  stage w h gamestate 
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
