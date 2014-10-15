module Control where

import Model as M
import Vector as V


--
-- UPDATE
--
activate : M.Strip -> M.GameState -> M.GameState
activate strip gs =
    let aStrips = map (\s -> {s | active <- True }) gs.strips
    in {gs | strips <- aStrips}

translate : M.Strip -> M.GameState -> M.GameState
translate strip delta gs =
    let tStrips = map (\s -> {s | loc <- s.loc `V.plus` (M.gridDelta delta)}) gs.strips
    in {gs | strips <- tStrips}

release : M.Strip -> M.GameState -> M.GameState
release strip gs =
    let rStrips = map (\s   ->  {s | active <- False
                                ,    loc <- V.round strip.loc
                                }) gs.strips
    in {gs | strips <- rStrips}

update : M.Event -> M.State -> M.State
update event state = case state of
    M.GotoPlay  -> M.Play M.initialGame
    M.Play gs   -> case event of
        Just (strip, Lift)              -> M.Play <| activate strip gs
        Just (strip, MoveBy (dx,dy))    -> M.Play <| translate strip gs
        Just (strip, Release)           -> M.Play <| release strip gs
        _                               -> state




