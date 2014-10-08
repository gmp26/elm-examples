module StatefulSignals where

import Window
import Mouse

data Action = Click | Animate Int Int

data State = Start | Play Game

type Game = {animating : Bool, width: Int, height : Int }

initialState = Start

animationSignal : Signal Action
animationSignal = (\(w,h) -> Animate w h) <~ (keepWhen isAnimatingSignal (300,300) Mouse.position)

clickSignal : Signal Action
clickSignal = (\s -> Click) <~ Mouse.clicks

isAnimating : State -> Bool
isAnimating state = case state of
    Play game -> game.animating
    otherwise -> False  

isAnimatingSignal : Signal Bool
-- isAnimatingSignal = constant True
isAnimatingSignal = (always True) <~ stateSignal

eventSignal : Signal Action
eventSignal = merges    [ animationSignal
                        , clickSignal
                        ]

stateSignal : Signal State
stateSignal = foldp update initialState eventSignal


update : Action -> State -> State
update action state =
    case state of
        Start -> Play {animating = False, width = 0, height = 0}
        Play game -> case action of
            Click       -> Play {game | animating <- not game.animating}
            Animate w h -> Play {game | width <- w, height <- h}


main : Signal Element
main = asText <~ stateSignal
