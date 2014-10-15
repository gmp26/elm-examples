module Combo where

import Window
import Mouse

import Utils as U
import Model as M
import View as V
import Control as C
import DragAndDrop as DD


startClick : Signal M.Event
startClick = (always M.GotoPlay) <~ Mouse.clicks  

dragSignal : Signal (Maybe (M.Strip, DD.Action))
dragSignal = DD.trackMultiple Nothing V.hover.signal

-- there may be a clash here...
eventSignal : Signal M.Event
eventSignal = merges  [ startClick
                      , dragSignal
                      ]

main : Signal Element
main = V.render <~ Window.dimensions ~ foldp C.update M.initialState eventSignal
