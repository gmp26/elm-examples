module Tuples where

import Set as S
import Dict as D

type Digit = Int  -- 0..9

-- convert a multidigit Int to a list of its digits in given base
listDigits : Int -> Int -> [Digit]
listDigits n base =
    let listDigits' n acc = if n < base
                                then n :: acc 
                                else listDigits' (n // base) ((n % base) :: acc)
    in listDigits' n []

removeZeros : [Digit] -> [Digit]
removeZeros digits = filter (\d -> d /= 0) digits

sortedNonZeros : Int -> Int -> [Digit]
sortedNonZeros n base = listDigits n base |> removeZeros >> sort >> reverse

reachables : [Digit] -> [Int]
reachables digits = 
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

-- treating n as a list of digits, return n with all reachable totals
nReachables : Int -> Int -> ([Digit], [Int])
nReachables base n = 
    let sn = sortedNonZeros n base
    in (sn, sortedNonZeros n base
            |> reachables >> S.fromList >> S.toList >> sort)

dictReachables : Int -> Int -> [([Digit], [Int])]
dictReachables maxLength stripCount =
    let base = maxLength + 1
        n = base ^ stripCount
    in  map (nReachables base) [1..n]
                |> D.fromList
                |> D.toList
{--}
main : Element
main = [9,3,1] |> reachables >> S.fromList >> S.toList >> sort >> asText
--}
{--
main : Element
main = dictReachables 10 4
        |> D.fromList
        |> D.toList
        |> map asText
        |> flow down
--}
