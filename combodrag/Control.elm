module Control (update) where

import Model as M
import Model (Alignment (..), Strip, State (..), GameState, Event (..))
import Vector as V
import View (gridDelta)
import Utils (gsz)
import DragAndDrop as DD
import Debug (watch)

--
-- UPDATE
--
toGrid : Strip -> Strip
toGrid strip =
    let bb = M.box strip
        tl = bb.topLeft
        grid coord = (coord / gsz) |> round
    in  M.align TL (grid <| (V.x tl), grid <| (V.y tl)) strip

-- vertical align top with another strip
alignTop : Strip -> Strip -> Strip
alignTop strip toStrip =
  let bb = M.box toStrip
      at = V.scale (1/gsz) bb.topLeft |> V.round
  in M.align TL at strip

-- vertical align bottom with another strip
alignBottom : Strip -> Strip -> Strip
alignBottom strip toStrip =
  let bb = M.box toStrip
      at = V.scale (1/gsz) bb.bottomRight |> V.round
  in M.align BR at strip

-- vertical align to nearest top or bottom boundary
alignNearest : Strip -> Strip -> Strip
alignNearest strip toStrip = if (V.y strip.loc > V.y toStrip.loc) 
    then alignTop strip toStrip
    else alignBottom strip toStrip

setDragging : Bool -> Strip -> Strip
setDragging b s = {s | dragging <- b} 

-- bring a strip to the top of a display list -- i.e. make it the last one
-- also highlight it if it overlaps another
toTop : Strip -> [Strip] -> [Strip]
toTop strip strips =
    {strip | highlight <- (any (M.overlaps strip) rest)} ++ rest
        |> reverse

-- if a strip overlaps another, then place the larger under the smaller
-- and align them both to top or bottom according to which is closest.
dropOn : Strip -> [Strip] -> [Strip]
dropOn strip strips =
    let rest = filter (\s -> s.n /= strip.n) strips
        activeStrip = {strip | highlight <- False}
        (overlaps, disjoints) = partition (M.overlaps activeStrip) rest
        displayOrder = sortBy .n overlaps |> reverse
    in  case partition (\s -> s.n < activeStrip.n) displayOrder of
            ([],[])
                -> activeStrip :: rest
            (smaller, []) 
                ->  let aligned = alignNearest activeStrip (head::smaller)
                    in aligned :: (smaller ++ disjoints)
            (smaller, larger)
                ->  let aligned = alignNearest activeStrip (head::larger)
                    in larger ++ (aligned :: smaller) ++ disjoints

-- update strips within the GameState, applying first transform to
-- the given strip, and the second transform to the remainder
activate : Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
activate =     
    let aStrips = map (\s -> if s.n == strip.n then f s else f' s) gs.strips
    in {gs | strips <- strip `toTop` aStrips}


deactivate : Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
deactivate strip f gs =
    let aStrips = map (\s -> if s.n == strip.n then f s else s) gs.strips
    in {gs | strips <- strip `dropOn` aStrips}

----------------

startDrag : Strip -> GameState -> GameState
startDrag strip gs =
    activate strip (setDragging True) (setDragging False) gs

drag : Strip -> V.Vector Int -> GameState -> GameState
drag strip delta gs =
    let translate s = {s | loc <- s.loc `V.plus` (gridDelta delta)}
    in activate strip translate identity gs

stopDrag : Strip -> GameState -> GameState
stopDrag strip gs =
    deactivate strip (toGrid << setDragging False) identity gs

----------------

update : M.Event -> State -> State
update event state = case state of
    M.Start     -> M.Play M.initialGame

    M.Play gs   -> watch "state" <| case event of
        M.Drag (Just (strip, DD.Lift))           -> M.Play <| startDrag strip gs
        M.Drag (Just (strip, DD.MoveBy delta))   -> M.Play <| drag strip delta gs
        M.Drag (Just (strip, DD.Release))        -> M.Play <| stopDrag strip gs
        _   -> state




