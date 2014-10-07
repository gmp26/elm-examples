module ButtonTest where

import Mouse
import Graphics.Input as GI 

data Keys = Number Int | Plus | Minus | Clear

keys : GI.Input Keys
keys = GI.input Clear

calculator : Element
calculator =
    flow right [ GI.button keys.handle (Number 1) "1"
               , GI.button keys.handle (Number 2) "2"
               , GI.button keys.handle    Plus    "+"
               ]

render : State -> Element
render state = case state of
    Start       -> [markdown| Click to test |]
    otherwise   -> calculator

data Action = CLICK | KEY

data State   = Start | Test 
initialState = Start

clickAction = (always CLICK) <~ Mouse.clicks
keyAction   = (always KEY) <~ keys.signal
actionSignal = merge clickAction keyAction

update : Action -> State -> State
update action state = Test

main : Signal Element
main = render <~ foldp update initialState actionSignal 