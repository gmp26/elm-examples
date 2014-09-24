import Mouse
import Window
import Dict

main : Signal Element
main = lift2 scene Window.dimensions clickLocations

tileSize = 10
gridSize = 32

type Pos = (Int,Int)
type Grid = Dict.Dict Pos Int

cartesianProduct l1 l2 = let
      halfPairs = map (,) l2
   in concatMap (\x -> map (\f -> f x) halfPairs) l2

getCellValue : Grid -> Pos -> Int
getCellValue dict pos = Dict.getOrElse 0 pos dict

updateCell : Grid -> Pos -> (Pos, Int)
updateCell dict (x,y) = let
      influencers = [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
      sumValue = map (getCellValue dict) influencers |> foldr (+) 0
      evaluation x = case x of
          2 -> 1
          3 -> 2
          _ -> 0
  in  ((x,y), evaluation sumValue)

updateGrid : Grid -> Grid
updateGrid oldGrid = let
      allCoord = cartesianProduct [0..gridSize] [0..gridSize]
      assocList = map (updateCell oldGrid) allCoord
  in  Dict.fromList assocList

normalizeCoord (x,y) = (x // tileSize, y // tileSize)

mouseInput : Signal (Pos, Bool)
mouseInput = sampleOn (fps 10) (lift2 (,) Mouse.position Mouse.isDown)

clickLocations : Signal Grid
clickLocations = let
    addIfMouseDown (pos, mouseDown) dict = case mouseDown of
      True -> normalizeCoord pos |> \p -> Dict.insert p 2 dict
      False -> dict
   in foldp (\i -> updateGrid << addIfMouseDown i) Dict.empty mouseInput

scene : Pos -> Grid -> Element
scene (w,h) locs =
  let drawPentagon ((x,y), c) =
          square 10 |> filled (hsla (toFloat c) 0.9 0.6 0.7)
                    |> move (toFloat (x * tileSize) - toFloat w / 2, toFloat h / 2 - toFloat (y * tileSize))
      drawCoords = Dict.foldl (\k v l -> (k, v) :: l) [] locs
  in  layers [ collage w h (map drawPentagon drawCoords)
             , plainText "Click to initiate life." ]
