module Scheduler where

{--
  A reusable scheduler which acts on a list of 'a's.
--}
import Debug (log)

type Task a =   { time          : Time              -- time to run
                , action        : Time -> a -> a    -- the action to run on each 'a'
                , selector      : a -> Bool         -- which 'a's to update
                , name          : String
                }

{-| make a task -}
make : Time -> (Time -> a -> a) -> (a -> Bool) -> String -> Task a
make t action selector name = { time = t, action = action, selector = selector, name=name }

{-| Add a task to the tasks list -}
schedule : Task a -> [Task a] -> [Task a] 
schedule = (::)

{-| The perform scheduled actions for a given time.

    Peform updates pair consisting of a list of states 
    to update, and a todo list of tasks. Each task actionis executed at the scheduled time.

    Each task contains a selector which may be applied to the state list to determine
    which states to update. i.e. it allows tasks to target only certain states.
-}
performOne : Time -> (a, [Task a]) -> (a, [Task a])
performOne t (state, tasks) =
    let (ready, notReady) = partition (\task -> t >= task.time) tasks
    in case ready of
        []  ->  (state, tasks)
        _   ->  (foldr (\t task -> task.action t state) state ready, notReady)

perform : Time -> ([a], [Task a]) -> ([a], [Task a])
perform t (states, tasks) =
    let (ready, notReady) = partition (\task -> t >= task.time) tasks
    in case ready of
        []  ->  (states, tasks)
        _   ->  let stepStates task states =
                    map (\state -> if task.selector state
                            then 
                                let state' = task.action t state
                                    logger = log task.name (t, state, state')  
                                in state'
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
tasks = foldr (\t acc -> schedule (make t action selector) acc) [] [1001..1004]

main : Element
main = perform 1000 ([1,2,3,4], tasks)  -- [1,2,3,4] + 4 tasks
        |> perform 1001                 -- [1,-4,3,-16] + 3 tasks
        |> perform 1002                 -- [1,16,3,256] + 2 tasks
        |> perform 1003                 -- [1,-256,3,-65536] + 1 tasks
        |> perform 1004                 -- [1,-256,3,-65536] + 0 tasks
        |> asText
--}