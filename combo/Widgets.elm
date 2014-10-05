module Widgets where
{-| A module where we can develop the buttons and controls needed
    to interact with combo. 

    We'd like to replace or augment these with touch or drag gestures, but
    are currently stymied by 
    [#775](https://github.com/elm-lang/Elm/issues/775) and
    [#774](https://github.com/elm-lang/Elm/issues/747).

    There is a third problem with the elm-runtime.js provided by the Mac pkg 
    distribution as it does not contain the fix to 
    [#210](https://github.com/elm-lang/Elm/issues/210).

    I've applied the patch in combo/elm-runtime.js on line 10174 so core buttons
    keep their gradient background.
-}

-- Nothing here yet, still using core buttons.