Notes
=====

Reached an impasse with this design (@tag impasse in log) due to these bugs
https://github.com/elm-lang/Elm/issues/775
https://github.com/elm-lang/Elm/issues/747

The issue occurs in both collages and containers. 

For collages
------------
If you want to move individual element within a rectangle then the collage creates a
canvas for each element which has the size of the containing rectangle, 
and this masks elements lower in the displaystack.

For containers
--------------
Each call to container creates a new full size div and gives them (usually) a property
`pointer-events:auto`. This will stop events reaching layers below the top one, so only
one positioned element can be clickable at a time.

A provisional getaround is to use separate launch buttons.
