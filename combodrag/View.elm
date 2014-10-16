module View where

import Utils (tf, gridSize, gsz)
import Model as M
import Model (initialState, box)
import Text as T
import Vector as V
import Graphics.Input as GI

hover : GI.Input (Maybe M.Strip)
hover = GI.input Nothing

--
-- VIEW
--

-- convert a mouse delta to a grid delta -- the y axis inverts
gridDelta : V.Vector Int -> V.Vector Float
gridDelta (dx, dy) = (V.toFloat (dx, -dy))

background : Int -> Int -> Element
background w h = collage w h [rect (tf w) (tf h) |> filled (grey)]

bannerStyle : T.Style
bannerStyle =   { typeface = ["Helvetica Neue", "Verdana", "Arial", "sans_serif"]
                , height = Just 40
                , color  = white
                , bold   = True
                , italic = False
                , line   = Nothing
                }

overlay : M.Strip -> Int -> Element
overlay strip _ =   let isz = gridSize - 4
                        osz = gridSize - 2
                    in spacer isz isz
                        |> color white
                        |> opacity 0.2
                        |> container osz (osz+2) middle

{--
stripElement : M.Strip -> Element
stripElement strip =    [rect 1 (tf strip.n)
                            |> filled (M.color strip)
                            |> scale (tf gridSize)
                        ]  
                        |> collage gridSize (strip.n * gridSize)
--}                        
{--
stripElement : M.Strip -> Element
stripElement strip =    [ spacer gridSize (gridSize*strip.n)
                            |> color black
                        , container gridSize (gridSize*strip.n) middle <|
                            (spacer (gridSize-2) (gridSize*strip.n - 2)
                                                |> color (M.color strip))
                        , container gridSize (gridSize*strip.n) middle <|
                            flow down (map (overlay strip) [1..strip.n])
                        ] |> layers
--}
{--}
label : M.Strip -> Element
label strip = strip.n
                |> show
                |> T.toText
                |> T.color (if strip.n<6 then black else white)
                |> T.height (gsz*0.75)
                |> T.bold
                |> T.centered
                |> width gridSize

stripElement : M.Strip -> Element
stripElement strip =    [ spacer (gridSize-2) (gridSize*strip.n - 2)
                            |> color (M.color strip)
                        , flow down (map (overlay strip) [1..strip.n])
                        , label strip
                        ]   |> layers
                            |> container gridSize (gridSize*strip.n) middle
                            |> color black
--}

draggable : M.Strip -> Element
draggable strip = 
  let which s = if s then Just strip else Nothing
  in GI.hoverable hover.handle which (stripElement strip)

stripForm : M.Strip -> Form
stripForm strip = draggable strip
                    |> toForm
                    |> move (strip.loc)

renderGame : (Int, Int) -> M.GameState -> Element
renderGame (w,h) gs = gs.strips
                        --|> sortWith (\s1 s2 -> 
                        --    if s1.dragging 
                        --        then GT
                        --        else if s2.dragging
                        --            then LT
                        --            else EQ)  
                        |> map stripForm
                        |> collage w h 

render : (Int,Int) -> M.State -> Element
render (w,h) state = case state of
    M.Start     ->  flow outward 
                [ background w h
                , toText "Click to Start"
                    |> T.style bannerStyle
                    |> centered
                    |> container w h middle
                ]
    M.Play gs   ->  flow outward 
                [ background w h
                , renderGame (w,h) gs
                ]

-- test draw a stack of strips
main : Element
main =  let testDraw  = { strips = [1..10] |> map M.testStrip}
        in render (500,500) <| M.Play testDraw
