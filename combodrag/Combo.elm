module Combo where

import Window
import Mouse

import Utils as U
import Model as M
import View as V
import Control as C

startClick : Signal M.Event
startClick = (always M.GotoPlay) <~ Mouse.clicks  

eventSignal : Signal M.Event
eventSignal = merges    [ startClick 
                        ]

main : Signal Element
main = V.render <~ Window.dimensions ~ foldp C.update M.initialState eventSignal
