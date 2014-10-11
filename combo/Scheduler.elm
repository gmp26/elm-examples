module Scheduler where

{--
  A reusable scheduler.
--}

type Task a =   { time      : Time              -- time to run
                , action    : Time -> a -> a    -- the action to run
                , selector  : a -> Bool         -- what to update
                }

makeTask : Time -> (Time -> a -> a) -> (a -> Bool) -> Task a
makeTask t action selector = { time = t, action = action, selector = selector }

schedule : Task a -> [Task a] -> [Task a] 
schedule = (::)

act : Time -> (a, [Task a]) -> (a, [Task a])
act t (state, tasks) =
    let (ready, notReady) = partition (\task -> t >= task.time) tasks
    in case ready of
        []  ->  (state, tasks)
        _   ->  let stepState task state =
                    if task.selector state
                        then task.action t state
                        else state
                in (foldr stepState state ready, notReady)


{-- -- test

-- select any even states 
selector : Int -> Bool
selector a = a % 2 == 0

-- if selected, action subtracts the state (an Int) from current time
action : Time -> Int -> Int
action t n = (round t) - n

-- make tasks to execute at times 1000..1003
tasks = foldr (\t acc -> schedule (makeTask t action selector) acc) [] [1000..1003]

main : Element
main = act 999 (2, tasks)   -- (2,...)        (no tasks ready, 4 more tasks)
        |> act 1000         -- (998,...)      (1000 - 2, three more tasks)
        |> act 1001         -- (3,...)        (1001 - 998, two more tasks)
        |> act 1002         -- (3,...)        (odd: not selected, one more task)
        |> act 1003         -- (3,[])         (odd: not selected, no more tasks)
        |> asText
--}