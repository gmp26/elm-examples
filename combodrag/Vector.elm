module Vector where

import Basics as B

type Vector a = (a,a)
type IV = Vector Int
type FV = Vector Float

x : Vector a -> a
x = fst

y : Vector a -> a
y = snd

map : (a -> b) -> Vector a -> Vector b
map f v = (f <| x v, f <| y v)

toFloat : IV -> FV
toFloat iv = map B.toFloat iv

round : FV -> IV
round fv = map B.round fv

ceiling : FV -> IV
ceiling fv = map B.ceiling fv

floor : FV -> IV
floor fv = map B.floor fv

plus : Vector number -> Vector number -> Vector number
plus u v = (x u + x v, y u + y v)

minus : Vector number -> Vector number -> Vector number
minus u v = u `plus` neg v

scale : number -> Vector number -> Vector number
scale a v = (a * x v, a * y v)

neg : Vector number -> Vector number
neg = scale -1

dot : Vector number -> Vector number -> number
dot u v = (x u * x v) + (y u * y v)

abs : Vector Float -> Float
abs v = dot v v |> sqrt

dist : Vector Float -> Vector Float -> Float
dist u v = abs (u `minus` v)

