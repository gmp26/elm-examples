module Control where

import Model as M

--
-- UPDATE
--
update : M.Event -> M.State -> M.State
update event state = case event of
    M.GotoPlay  -> if state == M.Play M.initialGame then M.Start else M.Play M.initialGame
