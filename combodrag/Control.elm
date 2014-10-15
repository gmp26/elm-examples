module Control where

import Model as M
import Vector as V

--
-- UPDATE
--
modify: M.Strip -> (M.Strip -> M.Strip) -> M.GameState -> M.GameState
modify strip f gs =
    let aStrips = map (\s -> if s == strip then f s else identity) gs.strips
    in {gs | strips <- aStrips}

startDrag : M.Strip -> M.GameState -> M.GameState
startDrag strip gs = 
    modify strip (\s -> {s | active <- True }) gs

drag : M.Strip -> M.GameState -> M.GameState
drag strip delta gs =
    modify strip (\s -> {s | loc <- s.loc `V.plus` (M.gridDelta delta)}) gs

stopDrag : M.Strip -> M.GameState -> M.GameState
stopDrag strip gs =
    modify strip (\s -> {s | active <- False, loc <- V.round strip.loc}) gs

update : M.Event -> M.State -> M.State
update event state = case state of
    M.GotoPlay  -> M.Play M.initialGame
    M.Play gs   -> case event of
        Just (strip, Lift)              -> M.Play <| startDrag strip gs
        Just (strip, MoveBy (dx,dy))    -> M.Play <| drag strip gs
        Just (strip, Release)           -> M.Play <| stopDrag strip gs
        _                               -> state




