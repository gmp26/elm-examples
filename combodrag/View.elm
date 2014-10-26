module View where
{--
module View (render, gridDelta, hover) where
--}

import Utils (tf, gridSize, gsz)
import Model as M
import Model (initialState, initialGame)
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

--rulerStyle = { defaultLine | color <- white, width <- 5 }
--tickStyle = {rulerStyle | width <- 3}

--tick : Int -> Form
--tick n = path [V.scale gsz (0.5, tf n), V.scale gsz (-0.5, tf n)] |> traced tickStyle

--ruler : Int -> Int -> Element
--ruler w h =   [ rect (2*gsz) (tf h) 
--                |> filled darkGrey
--                |> moveX -(gsz/2)
--            --, path [V.scale gsz (0,-10), V.scale gsz (0,10)] |> traced rulerStyle
--            , map tick [-20..20] |> group
--            , "drag to ruler"
--                |> T.toText
--                |> T.color white
--                |> T.height (gsz*0.75)
--                |> T.centered
--                |> width h
--                |> toForm
--                |> rotate (pi/2)
--                |> move (-gsz, 0)
--            ] |> collage w h

zoneElement : Int -> Int -> Float -> M.TLBR -> Element
zoneElement w h n tlbr =
    let rw = abs((V.x tlbr.bottomRight)-(V.x tlbr.topLeft))
        rh = abs((V.y tlbr.bottomRight)-(V.y tlbr.topLeft))
    in  [ rect rw rh 
            |> filled white
            |> move ((n-0.5)*gsz, 0)
        ] |> collage w h

leftZone : Int -> Int -> M.TLBR
leftZone w h =
    let tl = ( 0.3*(tf w), (0.5)*(tf h) )
    in  { topLeft = tl
        , bottomRight = ( gsz + fst tl - 2, (-0.5)*(tf h) )
        }

rightZone : Int -> Int -> M.TLBR
rightZone = leftZone

leftZoneElement : Int -> Int -> Element
leftZoneElement w h = leftZone w h |> zoneElement w h -1

rightZoneElement : Int -> Int -> Element
rightZoneElement w h = rightZone w h |> zoneElement w h 0

baseElement : Int -> Int -> Element
baseElement w h =
    let w2 = (tf w) / 2
        h2 = (tf h) / 2
        baseStyle = defaultLine
    in  [   [ rect (tf w) (tf h)
                |> filled darkGrey
            , segment (-w2, h2) (w2, h2) 
                |> traced baseStyle
            ]   |> group
                |> move (0, -h2 - (9 * gsz))
        ] |> collage w h
          |> opacity 0.7

background : Int -> Int -> Element
background w h =
    let hf = tf h 
    in  [ rect (tf w) hf |> filled (M.backgroundColor)
        ]   |> collage w h

bannerStyle : T.Style
bannerStyle =   { typeface = ["Helvetica Neue", "Verdana", "Arial", "sans_serif"]
                , height = Just 40
                , color  = white
                , bold   = True
                , italic = False
                , line   = Nothing
                }

overlay : M.Strip -> Int -> Element
overlay strip _ =   let isz = gridSize - 3
                        osz = gridSize - 2
                    in spacer isz isz
                        |> color white -- (if strip.n==7 then charcoal else white)
                        |> opacity (if strip.n==7 then 0.5 else 0.3)
                        |> width (osz-3)
                        |> height (osz-3)
                        |> container (osz) (osz+2) middle

label : M.Strip -> Element
label strip = strip.n
                |> show
                |> T.toText
                |> T.typeface ["Helvetica Neue", "Verdana", "Arial", "sans_serif"]
                |> T.color (if strip.n<6 then black else white)
                |> T.height (gsz*0.75)
                |> T.bold
                |> T.centered
                |> width gridSize

stripElement : M.Strip -> Element
stripElement strip =    [ spacer (gridSize-1) (gridSize*strip.n - 2)
                            |> color (M.color strip)
                        , flow down (map (overlay strip) [1..strip.n])
                        , label strip
                        ]   |> layers
                            |> container (gridSize+1) (gridSize*strip.n+1) bottomRight
--                            |> color (black)



draggable : M.Strip -> Element
draggable strip = 
  let which s = if s then Just strip else Nothing
  in GI.hoverable hover.handle which (stripElement strip)

stripForm : M.Strip -> Form
stripForm strip = draggable strip
                    |> toForm
                    |> move (strip.loc)

renderGame : Int -> Int -> M.GameState -> Element
renderGame w h gs = gs.strips
                        |> map stripForm
                        |> collage w h 

render : (Int,Int) -> M.State -> Element
render (w,h) state = 
    let 
        f = (tf w)/640
        gw = 640
        gh = 960
    in  [   (case state of
                M.Start     ->  flow outward 
                                    [ background gw gh
                                    , toText "Click to Start"
                                        |> T.style bannerStyle
                                        |> centered
                                        |> container gw gh middle
                                    ]
                M.Play gs   ->  flow outward 
                                    [ background gw gh
                                    , leftZoneElement gw gh
                                    , rightZoneElement gw gh
                                    , baseElement gw gh
                                    , renderGame gw gh gs
                                    ]
            )
                |> toForm
                |> scale f
        ] |> collage w h  


-- test draw a stack of strips
--testStrip : Int -> M.Strip
--testStrip n = M.align M.BR (n-5,-5) {n = n, loc = (0, 0), dragging = False}
main : Element
main = 
    let testDraw  = {initialGame |  strips <- ([1..10] |> map M.testStrip) }
    in render (500,500) <| M.Play testDraw
