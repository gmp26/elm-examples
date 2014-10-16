module Combo where

import Window
import Mouse

import Utils as U
import Model as M
import View as W
import View (hover)
import Control as C
import DragAndDrop as DD


dragSignal : Signal (M.Event)
dragSignal = (\dd -> M.Drag dd) <~ DD.trackMultiple Nothing hover.signal

-- there may be a clash here...
eventSignal : Signal M.Event
eventSignal = merges  [ dragSignal
                      ]

main : Signal Element
main = W.render <~ Window.dimensions ~ foldp C.update M.initialState eventSignal
