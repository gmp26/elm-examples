module Control where

import Model as M
import Vector as V
import View (gridDelta)
import DragAndDrop as DD
import Debug (log)

--
-- UPDATE
--
modify: M.Strip -> (M.Strip -> M.Strip) -> (M.Strip -> M.Strip) -> M.GameState -> M.GameState
modify strip f f' gs =
    let aStrips = map (\s -> if s.n == strip.n then f s else f' s) gs.strips
    in {gs | strips <- aStrips}

-- bring a strip to the top of the display stack
toTop : M.Strip -> M.GameState -> M.GameState
toTop strip gs =
    let p = partition (\s -> s /= strip {-not s.dragging-}) gs.strips
    in {gs | strips <- (fst p) ++ (snd p)}

startDrag : M.Strip -> M.GameState -> M.GameState
startDrag strip gs =
    modify strip (M.setDragging True) (M.setDragging False) (toTop strip gs)

drag : M.Strip -> V.Vector Int -> M.GameState -> M.GameState
drag strip delta gs =
    modify strip (\s -> {s | loc <- s.loc `V.plus` (gridDelta delta)}) identity (toTop strip gs)

stopDrag : M.Strip -> M.GameState -> M.GameState
stopDrag strip gs =
    modify strip (M.toGrid << M.setDragging False) identity (toTop strip gs)

update : M.Event -> M.State -> M.State
update event state = case state of
    M.Start     -> M.Play M.initialGame

    M.Play gs   -> case event of
        M.Drag (Just (strip, DD.Lift))           -> M.Play <| startDrag strip gs
        M.Drag (Just (strip, DD.MoveBy delta))   -> M.Play <| drag strip delta gs
        M.Drag (Just (strip, DD.Release))        -> M.Play <| stopDrag strip gs
        _   -> state



