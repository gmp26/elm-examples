module GameSkeleton where

import Window

-- INPUTS
type UserInput = {}

userInput : Signal UserInput
userInput = constant {}

type Input = { timeDelta:Float, userInput:UserInput }


-- MODEL
type GameState = {}

defaultGame : GameState
defaultGame = {}


-- UPDATE
stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = gameState


-- DISPLAY
display : (Int,Int) -> GameState -> Element
display (w,h) gameState = asText gameState


-- PLUMBING
delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState