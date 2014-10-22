module Tuples (reachables) where

import Set as S
import Dict as D

type Digit = Int  -- 0..9

reachables' : [Digit] -> [Int]
reachables' digits = 
    case digits of
        []              ->  []
        (x :: [])       ->  [x]
        (x :: y :: zs)  ->
            let reachables' = reachables (y :: zs)
                listof x = repeat (length reachables') x
                xAndReachables x = zipWith (+) (listof x) reachables'
                diff a b = a - b |> abs
                xDiffReachables x = zipWith diff (listof x) reachables'
            in [x + y, x - y, x] ++ reachables' ++ (xAndReachables x) ++ (xDiffReachables x)


reachables : [Digit] -> [Int]
reachables = sort >> reverse >> reachables' >> S.fromList >> S.toList >> sort

{--
main : Element
main = [1,2,5,10] |> reachables >> asText
--}
