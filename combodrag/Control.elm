module Control where

import Model as M
import Vector as V
import View (gridDelta)
import DragAndDrop as DD

--
-- UPDATE
--
modify: M.Strip -> (M.Strip -> M.Strip) -> M.GameState -> M.GameState
modify strip f gs =
    let aStrips = map (\s -> if s == strip then f s else s) gs.strips
    in {gs | strips <- aStrips}

startDrag : M.Strip -> M.GameState -> M.GameState
startDrag strip gs = 
    modify strip (\s -> {s | dragging <- True }) gs

drag : M.Strip -> V.Vector Int -> M.GameState -> M.GameState
drag strip delta gs =
    modify strip (\s -> {s | loc <- s.loc `V.plus` (gridDelta delta)}) gs

stopDrag : M.Strip -> M.GameState -> M.GameState
stopDrag strip gs =
    modify strip (\s -> {s | dragging <- False, loc <- strip.loc}) gs

update : M.Event -> M.State -> M.State
update event state = case state of
    M.Start     -> M.Play M.initialGame

    M.Play gs   -> case event of
        M.Drag (Just (strip, DD.Lift))           -> M.Play <| startDrag strip gs
        M.Drag (Just (strip, DD.MoveBy delta))   -> M.Play <| drag strip delta gs
        M.Drag (Just (strip, DD.Release))        -> M.Play <| stopDrag strip gs
        _   -> state




