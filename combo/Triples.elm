module Tuples where

import Set as S

type Digit = Int  -- 0..9

-- convert a multidigit Int to a list of its digits
listDigits : Int -> [Digit]
listDigits n =
    let listDigits' n acc = if n < 10
                                then n :: acc 
                                else listDigits' (n // 10) ((n % 10) :: acc)
    in listDigits' n []

removeZeros : [Digit] -> [Digit]
removeZeros digits = filter (\d -> d /= 0) digits

sortedNonZeros : Int -> [Digit]
sortedNonZeros n = listDigits n |> removeZeros >> sort >> reverse

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
nReachables : Int -> Element
nReachables n = (n, sortedNonZeros n
                    |> reachables >> S.fromList >> S.toList >> sort)
                    |> asText

main : Element
main =  flow down <| map nReachables [1..999]