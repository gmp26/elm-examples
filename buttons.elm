module Main where

import Debug
import Graphics.Input (..)

data Keys = Number Int | Plus | Minus | Clear

keys : Input Keys
keys = input Clear

--calculator : Element
calculator kp = 
    flow right [ button keys.handle (Number 1) "1"
               , button keys.handle (Number 2) "2"
               , button keys.handle    Plus    "+"
               ]

keyPress = Debug.watch "key" <~ keys.signal

main = calculator <~ keyPress