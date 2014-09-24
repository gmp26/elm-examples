--license: CC-0
--see: https://creativecommons.org/publicdomain/zero/1.0/

import Mouse
import Window

type Branch = { x1:Float
              , y1:Float
              , x2:Float
              , y2:Float
              , w:Float  -- line width
              , a:Float  -- angle
              , l:Int    -- level: i.e. the number of parents
              }

-- compute the length of a branch
len : Branch -> Float
len {x1, y1, x2, y2} = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- create a child branch from a parent
branch : Branch -> (Float -> Float -> Float) -> Float -> Float -> Branch
branch p fn delta_a sc = 
    { x1 = p.x2
    , y1 = p.y2 
    , x2 = p.x2 + (len p) * sc * cos (degrees (fn p.a delta_a))
    , y2 = p.y2 + (len p) * sc * sin (degrees (fn p.a delta_a))
    , w  = p.w * 0.75
    , a  = fn p.a delta_a
    , l  = p.l + 1
    }

-- draw a branch
drawBranch : Branch -> Form
drawBranch {x1, y1, x2, y2, w} = traced  {defaultLine | width <- w} <| path [(x1,y1),(x2,y2)]

branches : Branch -> (Float -> Float -> Float) -> Float -> Float -> [Branch]
branches p fn delta_a sc = if p.l < 10
                           then let relTrunk = branch p fn delta_a sc
                                in  relTrunk :: (branches relTrunk (+) delta_a sc) ++ (branches relTrunk (-) delta_a sc)
                           else [] 

trunk : Branch
trunk = {x1 = 0, y1 = 0, x2 = 0, y2 = 50, w = 10, a = 90, l = 0}

tree : Float -> Float -> [Branch]
tree delta_a sc = (branches trunk (+) delta_a sc) ++ [trunk] ++ (branches trunk (-) delta_a sc)

fraction1 = lift2 (\x w -> toFloat x / toFloat w) Mouse.x Window.width
fraction2 = lift2 (\y h -> 1.0 - toFloat y / toFloat h) Mouse.y Window.height

main =
  let scene frac1 frac2 = collage 700 700 <| map drawBranch <| tree (90 * frac1) frac2
  in  lift2 scene fraction1 fraction2


