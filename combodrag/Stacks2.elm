module Stacks2 where

import Either as E
import Either (Either (..), lefts, rights)
import Utils (uniq)

type Numbered a = {a | n: Int}

type Entry a = Either (Numbered a) (Numbered a)
type Entries a = [Entry a]

{-
    add a new entry to left or right stack
-}
add: Entry a -> Entries a -> Entries a
add = (::)

{- 
    Heights of a numberd stack
    stackHeights [1,4,7] = [0,1,5,12]
-}
stackHeights : [Numbered a] -> [Int]
stackHeights s = scanl (\na x -> x + na.n) 0 (reverse s)

{- 
    heightsOf lefts [Left 1, Right 3, Left 9] == [0,1,10]
-}
heightsOf : (Entries a -> [Numbered a]) -> Entries a -> [Int]
heightsOf onSide = onSide >> stackHeights

{-
    all the heights of entries on both sides
    allHeights ([1,4,7],[3,5]) == [0,1,3,5,8,12]
-}
allHeights: Entries a -> [Int]
allHeights = (\s -> (heightsOf lefts s) ++ (heightsOf rights s)) >> uniq >> sort

allLengths: Entries a -> [Int]
allLengths entries =
    let ah = allHeights entries
    in case ah of
        [] -> []
        otherwise -> 
            let bah = take ((length ah) - 1) ah
            in  (drop 1 ah)
                    |> map (\x -> bah
                                    |> map (\y -> abs(x - y)))
                |> concat
                |> uniq


main : Element
main = []
        |> add (Left {n=1})
        |> add (Right {n=3})
        |> add (Right {n=5})
        |> add (Left {n=4})
        |> add (Left {n=7})
        |> allLengths
        |> asText