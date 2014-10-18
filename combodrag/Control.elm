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

setDragging : Bool -> Strip -> Strip
setDragging b s = {s | dragging <- b} 

-- bring a strip to the top of a display list -- i.e. make it the last one
-- also highlight it if it overlaps another
toTop : Bool -> Strip -> [Strip] -> [Strip]
toTop sense strip strips =
    let (rest, [activeStrip]) = partition (\s -> s.n /= strip.n) strips
    in rest ++ [if (any (M.overlaps activeStrip) rest)
                    then {activeStrip | highlight <- sense}
                    else {activeStrip | highlight <- False}]

-- update strips within the GameState, applying first transform to
-- the given strip, and the second transform to the remainder
modify: Bool -> Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
modify sense strip f f' gs =
    let aStrips = map (\s -> if s.n == strip.n then f s else f' s) gs.strips
    in {gs | strips <- toTop sense strip aStrips}

activate : Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
activate = modify True

deactivate : Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
deactivate = modify False


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




