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

act : Time -> ([a], [Task a]) -> ([a], [Task a])
act t (states, tasks) =
    let (ready, notReady) = partition (\task -> t >= task.time) tasks
    in case ready of
        []  ->  (states, tasks)
        _   ->  let stepStates task states =
                    map (\state -> if task.selector state
                            then task.action t state
                            else state
                        ) states
                in (foldr stepStates states ready, notReady)


{-- -- test

-- select even states 
selector : Int -> Bool
selector a = a % 2 == 0

-- if selected, action squares the state, negating it if the time is odd.
action : Time -> Int -> Int
action t n = n * n * (if (round t) % 2 == 0 then 1 else -1)

-- schedule tasks to execute the test action at times 1001..1004 on even states.
tasks = foldr (\t acc -> schedule (makeTask t action selector) acc) [] [1001..1004]

main : Element
main = act 1000 ([1,2,3,4], tasks)  -- [1,2,3,4] + 4 tasks
        |> act 1001                 -- [1,-4,3,-16] + 3 tasks
        |> act 1002                 -- [1,16,3,256] + 2 tasks
        |> act 1003                 -- [1,-256,3,-65536] + 1 tasks
        |> act 1004                 -- [1,-256,3,-65536] + 0 tasks
        |> asText
--}