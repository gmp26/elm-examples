import Graphics.Input as Input
import Mouse

clicked = Input.input 0

rectangle : Color -> Element
rectangle c = [rect 50 40 |> filled c] |> collage 50 40 

rect1 = Input.hoverable clicked.handle (\b -> if b then 1 else 0) (rectangle red)
rect2 = Input.hoverable clicked.handle (\b -> if b then 2 else 0) (rectangle green)

initialState = ((-50, 0), (50, 0))

tfPair : (Int,Int) -> (Float, Float)
tfPair (m,n) = (toFloat <| m - 200, toFloat <| 200 - n)

step (down, start, pos, rect) state = 
    if down 
        then if | rect == 2 -> (fst state, tfPair pos)
                | rect == 1 -> (tfPair pos, snd state)
                | otherwise -> state
        else state

inputs = (,,,) <~ Mouse.isDown ~ (sampleOn Mouse.isDown Mouse.position) ~ Mouse.position ~ (sampleOn Mouse.isDown clicked.signal)

state = foldp step initialState inputs

display offsets x = collage 400 400 [toForm rect1 |> move (fst offsets), toForm rect2 |> move (snd offsets)] `above` asText x

main = display <~ state ~ clicked.signal 


