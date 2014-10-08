module StatefulSignals where
{-
  This code tests whether it's possible to use a stateful signal -- one derived from
  the foldp output -- to throttle inputs. The answer appears to be 'No'.

  1. There's a discussion in 
     [Elm: Concurrent FRP for Functional GUIs](Elm: Concurrent FRP for Functional GUIs) in
     section 4.2 which indicates Elm signal graphs are directed acyclic graphs.

  2. The code below will fail if line 27 is swapped for line 28, which violates 1.
-}
import Window
import Mouse

data Action = Click | Animate Int Int
type State = {animating : Bool, width: Int, height : Int }

initialState = {animating = False, width = 300, height = 300}

animationSignal : Signal Action
animationSignal = (\(x,y) -> Animate x y) <~ (keepWhen isAnimatingSignal (300,300) Mouse.position)

clickSignal : Signal Action
clickSignal = (\s -> Click) <~ Mouse.clicks

isAnimatingSignal : Signal Bool
isAnimatingSignal = constant True
-- isAnimatingSignal = (always True) <~ stateSignal

actionSignal : Signal Action
actionSignal = merge animationSignal clickSignal

stateSignal : Signal State
stateSignal = foldp update initialState actionSignal

update : Action -> State -> State
update action state =
    case action of
        Click       -> {state | animating <- not state.animating}
        Animate x y -> {state | width <- x, height <- y}

main : Signal Element
main = asText <~ stateSignal
