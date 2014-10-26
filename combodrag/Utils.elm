module Utils (tf, gridSize, gsz, takeWhile, uniq) where

import Set as S

--
-- HELPERS
--
tf = toFloat

gridSize : Int
gridSize = 40

gsz : Float
gsz = tf gridSize

takeWhile : (a -> Bool) -> [a] -> [a]
takeWhile p aList =
    if  isEmpty aList
        then []
        else let (x :: xs) = aList
             in if p x then (takeWhile p xs) else []

uniq : [comparable] -> [comparable]
uniq = S.fromList >> S.toList