import Mouse
import Graphics.Input as Input

-- Version for 0.12.3 server and local copy of elm-runtime --


{-
-- HELPERS
-}
tf = toFloat

{-
-- INPUTS
-}
data Command = Play | Clear

commands : Input.Input Command
commands = Input.input Clear

buttonSize = 100

txt : Float -> Color -> String -> Element
txt p clr string =
    leftAligned . (Text.height (p * buttonSize)) .
    typeface ["Helvetica Neue","Sans-serif"] . (Text.color clr) <| toText string


button : Color -> Color -> Int -> Int -> Command -> String -> Element
button background foreground w h command name =
    let n = min w h
        btn alpha =
            layers [ color black . container w h middle .
                     color background . container (w-2) (h-2) middle .
                     container (2*n) n middle <| txt 0.4 foreground name
                   , color (rgba 0 0 0 alpha) (spacer w h)
                   ]
    in  Input.customButton commands.handle command (btn 0) (btn 0.05) (btn 0.1)

lightButton : Int -> Int -> Command -> String -> Element
lightButton = button lightBlue white

startButton : Element
-- startButton = Input.button commands.handle Play "Play"
startButton = lightButton 200 100 Play "Play Nim"


{-
-- MODEL
-}
data Player = A | B | C

data Screen = Intro | Playing Game | GameOver

type Game   = { counters : Int
              , selected : Maybe Int
              , turn : Player
              }

initialGame = { counters = initialCount
              , selected = Nothing
              , turn = A
              }

{-
-- DISPLAY
-}
initialCount : Int
initialCount = 7

maxCount : Int
maxCount = initialCount

units : Int
units = 6

counterRadius : Float
counterRadius = 10 * (tf units)

counterSize : Int
counterSize = 2 * (ceiling counterRadius)

gapSize : Int
gapSize = 1 * units

counterElement : Bool -> Element
counterElement faded = 
  opacity (if faded then 0.2 else 1)
  <| collage counterSize counterSize 
  <| [filled red <| circle counterRadius]

gap : Element
gap = spacer 5 5

-- Draw m counters in a row, faded out from the nth counter
counterRow : Int -> Int -> Element
counterRow m n =  flow right
                  <| intersperse gap 
                  <| map (\i -> counterElement (i >= n)) [1..m]

showHelp : Element
showHelp = startButton

showOver : Element
showOver = [markdown|#Game Over|]

showGame : Game -> Element
showGame g = flow down
  [ plainText ("Player " ++ (show g.turn) ++ " to move")
  , counterRow 5 3
  ] 

display : Screen -> Element
display screen = case screen of
  Intro -> showHelp
  (Playing g) -> showGame g
  GameOver -> showOver 

{-
-- UPDATE
-}
data Event = StartGame | NewGame | Selection (Maybe Int)

switchPlayer : Player -> Player
switchPlayer turn = case turn of
                      A -> B
                      B -> A
                      C -> C

introEvent : Event -> Screen -> Screen
introEvent event screen = case event of
  StartGame -> Playing initialGame
  otherwise -> screen

overEvent : Event -> Screen -> Screen
overEvent event screen = case event of
  NewGame   -> Playing initialGame
  otherwise -> screen

playEvent : Game -> Event -> Screen -> Screen
playEvent game event screen = case event of
  (Selection (Just removeCount)) 
            ->  Playing { game |  counters <- game.counters - removeCount
                        , turn <- switchPlayer game.turn
                        }

  (Selection (Nothing))
            -> Playing game

  otherwise -> screen

update : Event -> Screen -> Screen
update event screen = case screen of
  Intro           -> introEvent event screen
  GameOver        -> overEvent event screen
  (Playing game)  -> playEvent game event screen

{-
-- PLUMBING
-}

startSignal = (always StartGame) <~ commands.signal

eventSignal : Signal Event
eventSignal = startSignal

screenSignal : Signal Screen
screenSignal = foldp update Intro startSignal

--main = startButton

main : Signal Element
main = lift display screenSignal 