module Control (update) where

import Model as M
import Model (Alignment (..), Strip, State (..), GameState, Event (..))
import Vector as V
import View (gridDelta)
import Utils (gsz)
import DragAndDrop as DD
import Set as S
import Debug (watch, log)

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
alignTop toStrip strip =
  let bb = M.box toStrip
      at = V.scale (1/gsz) bb.topLeft |> V.round
  in M.align TL at strip

-- TODO: switch first and second parameters of alignment functions

-- vertical align bottom with another strip
alignBottom : Strip -> Strip -> Strip
alignBottom toStrip strip =
  let bb = M.box toStrip
      at = V.scale (1/gsz) bb.bottomRight |> V.round
  in M.align BL at strip

-- vertical align to nearest top or bottom boundary
alignNearest' : Bool -> Strip -> Strip -> Strip
alignNearest' top toStrip strip = if (top `xor` (V.y strip.loc > V.y toStrip.loc))
    then alignTop toStrip strip 
    else alignBottom toStrip strip

alignSmaller : Strip -> Strip -> Strip
alignSmaller = alignBottom -- alignNearest' True

alignLarger : Strip -> Strip -> Strip
alignLarger = alignBottom -- alignNearest' False

setDragging : Bool -> Strip -> Strip
setDragging b s = {s | dragging <- b} 

-- bring a strip to the top of a display list -- i.e. make it the last one
-- also highlight it if it overlaps another
toTop : Strip -> [Strip] -> [Strip]
toTop strip strips =
    case partition (\s -> s.n /= strip.n) strips of
        (rest, [activeStrip]) ->  
            rest ++ [if (any (M.overlaps activeStrip) rest)
                        then {activeStrip | highlight <- True}
                        else {activeStrip | highlight <- False}]
        otherwise -> strips


data Affected a = Adjacent [a] | Overlap [a]
type DropInfo a = {dropped : a, affected : Maybe (Affected a), rest : [a]}

-- categorize the strips by drop location
getDropInfo : Strip -> [Strip] -> DropInfo Strip
getDropInfo strip strips = 
    let (rest, [strip']) = partition (\s -> s.n /= strip.n) strips
        (overlaps', rest') = partition (M.overlaps strip') rest
        (adjacents, disjoints) = partition (M.aboveOrBelow strip') rest'
        overlaps = sortBy .n overlaps' |> reverse

    in  { dropped   =   {strip' | highlight <- False}
        , affected  =   if  | not (isEmpty overlaps)  -> Just <| Overlap overlaps
                            | not (isEmpty adjacents) -> Just <| Adjacent adjacents
                            | otherwise             -> Nothing
        , rest      = rest'
        }

{-
-- A strip has been dropped on one or more other strips.
-- The strip will be aligned with the top or bottom of the underlying strips,
-- whichever is nearer.
-- The display order will ensure all strips are visible (largest at bottom).
-- New difference measures will be calculated.
-}
dropOver : Strip -> [Strip] -> [Strip] -> GameState -> GameState
dropOver dropped overlaps rest gs =
    case partition (\s -> s.n < dropped.n) overlaps of  
        ([],[])
            ->  { gs |  strips <- dropped :: rest}
        (smaller, []) 
            ->  let aligned = alignSmaller (head smaller) dropped
                in  { gs | strips <- aligned :: (smaller ++ rest)
                    , reached <- addExtraOps aligned smaller rest gs.reached
                    }
        ([], larger)
            ->  let aligned = alignLarger (head larger) dropped
                in  { gs | strips <- larger ++ [aligned] ++ rest
                    , reached <- addExtraOps aligned larger rest gs.reached
                    }
        (smaller, larger)
            ->  let aligned = alignLarger (head larger) dropped
                in  { gs | strips <- larger ++ (aligned :: smaller) ++ rest
                    , reached <- addExtraOps aligned (smaller++larger) rest gs.reached
                    }

-- Augment the current reached numbers with newly reached ones
addExtraOps : Strip -> [Strip] -> [Strip] -> [Int] -> [Int]
addExtraOps strip overlaps rest reached =
    let sums  = (map (\s -> abs <| s.n - strip.n) overlaps)
        diffs = if  not (isEmpty rest || isEmpty overlaps)
                then 
                    let r = head rest
                        o = head overlaps
                    in if r `M.overlaps` o
                        then [abs(abs(r.n - o.n) - strip.n)]
                        else []
                else []
    in diffs ++ sums ++ reached
        |> S.fromList
        |> S.toList
        |> sort


-- if a strip overlaps another, then place the larger under the smaller
-- and align them both to top or bottom according to which is closest.
dropOn : Strip -> GameState -> GameState
dropOn strip gs =
    let {dropped, affected, rest} = getDropInfo strip gs.strips
    in watch "state" <| case affected of
        Just (Overlap overlaps)
            -> dropOver dropped overlaps rest gs
        otherwise
            -> gs


-- update strips within the GameState, applying first transform to
-- the given strip, and the second transform to the remainder
-- if a strip drops on the ruler, it will (somehow) trigger new measurements
activate : Strip -> (Strip -> Strip) -> (Strip -> Strip) -> GameState -> GameState
activate strip f1 f2 gs =     
    let aStrips = map (\s -> if s.n == strip.n then f1 s else f2 s) gs.strips
    in {gs | strips <- toTop strip aStrips}


deactivate : Strip -> (Strip -> Strip) -> GameState -> GameState
deactivate strip f gs =
    let aStrips = map (\s -> if s.n == strip.n then f s else s) gs.strips
    in strip `dropOn` {gs | strips <- aStrips}

    

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
    deactivate strip (toGrid << setDragging False) gs

----------------

update : M.Event -> State -> State
update event state = case watch "state" <| state of
    M.Start     -> M.Play M.initialGame

    M.Play gs   -> case watch "event" event of
        M.Drag (Just (strip, DD.Lift))          -> M.Play <| startDrag strip gs
        M.Drag (Just (strip, DD.MoveBy delta))  -> M.Play <| drag strip delta gs
        M.Drag (Just (strip, DD.Release))       -> M.Play <| stopDrag strip gs
        M.Resize (w,h)                          -> M.Play <| {gs| w <- w, h <- h}
        _   -> state




