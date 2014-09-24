import Keyboard
import Text
import Random

-- DATA

---- State

{-| State of the game. Each playing state keeps track of the level -}
data GameState = 
  Intro       |
  Won     Int |
  Lost    Int |
  Playing Int ShipState [AstState] [BulletState] [AstExplode]


---- Game objects

type Movable a = { a | pos : (Float, Float), dir : Float, speed: Float }

type ShipState = Movable {}

type AstState = Movable { diam : Float, face : Float }

type AstExplode = { pos : (Float, Float), diam : Float, face : Float, life : Int }
                    
type BulletState = Movable {}


---- Input handling

data GameInput = GameInput Int KeyInput

type KeyInput = {space : Bool, arrows : { x:Int, y:Int }}


                     
-- GUI

{-| Render the ship -}
showShip : ShipState -> Form
showShip state =
  showPlane state.pos state.dir

showPlane : (Float, Float) -> Float -> Form
showPlane pos orientation =
  polygon [(10,0),(-8,-5),(-4,0),(-8,5)]
    |> filled white
    |> rotate orientation  
    |> move pos

{-| Render an asteroid -}
showAst : AstState -> Form
showAst state =
  showAsteroid state.pos state.diam state.face

showAsteroid : (Float, Float) -> Float -> Float -> Form
showAsteroid pos diam dir =
  ngon 5 (diam+4)
    |> filled white
    |> move pos
    |> rotate dir

{-| Render an exploding asteroid -}
showExplosion : AstExplode -> Form
showExplosion expl = 
  ngon 5 (expl.diam + 4 * toFloat (expl.life % 3)) 
    |> outlined { defaultLine | width <- 3, color <- white }
    |> rotate (expl.face + toFloat expl.life) 
    |> move expl.pos

{-| Render a bullet -}
showBullet : BulletState -> Form
showBullet bull = 
  oval 3 3 -- 1 px is too small
    |> filled white
    |> move bull.pos

{-| A big black rect -}
background =
  rect 500 500 
    |> filled black

{-| Render the game state -}
scene : GameState -> Element 
scene game = 
  let
    whiteText s = toText s |> Text.color white |> centered
    title s = [background, whiteText s |> toForm]
    stuff = case game of
      Intro   -> title "ASTEROIDS\n___\n\nVINCENT GOOSSENS"
      Won n   -> title ("YOU WON\n___\n\nNEXT UP: ROUND " ++ show (n+1))
      Lost n  -> title ("YOU DIED\n___\n\nYOU MADE IT TO: ROUND " ++ show n)
      Playing _ ship asts bulls expls -> 
        [background
        , showShip ship]
        ++ (map showAst asts)
        ++ (map showBullet bulls)
        ++ (map showExplosion expls)
  in 
    collage 500 500 stuff
    
    

-- GAME LOGIC

---- Collisions

{-| True iff collision is detected -}
collision : Float -> (Float, Float) -> AstState -> Bool
collision offset pos ast =
  let
    (x1, y1) = pos
    (x2, y2) = ast.pos
    dist = ast.diam + offset
  in
    dist > sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        
{-| True iff ship hits ast -}
shipCollision : ShipState -> AstState -> Bool
shipCollision ship ast =
  collision 8 ship.pos ast

{-| True iff ast hits any bullet -}
bulletsAstCollision : [BulletState] -> AstState -> Bool
bulletsAstCollision bullets ast =
  any (\b -> collision 1 b.pos ast) bullets

{-| True iff bullet hits any ast -}
astsBulletCollision : [AstState] -> BulletState -> Bool
astsBulletCollision asts bullet =
  any (collision 1 bullet.pos) asts

{-| Checks for collisions with other asteroids and updates ast -}
astsAstCollision : [AstState] -> AstState -> AstState
astsAstCollision asts ast =
   let
     colliding = filter (\a -> a /= ast && (collision a.diam a.pos ast) ) asts
     bump a b =
       let 
         (ax, ay) = a.pos
         (bx, by) = b.pos
         dist = sqrt ((ax-bx)^2 + (ay-by)^2)
         angle = atan2 (ay-by) (ax-bx)
         vax = a.speed * cos (a.dir - angle)
         vay = a.speed * sin (a.dir - angle)
         vbx = b.speed * cos (b.dir - angle)
         vby = b.speed * sin (b.dir - angle)
         fax = (vax*(a.diam-b.diam) + (2*b.diam*vbx))/(a.diam+b.diam)
         fbx = (vbx*(a.diam-b.diam) + (2*b.diam*vax))/(a.diam+b.diam)
         va = sqrt (fax^2 + vay^2 )
         da = angle + atan2 vay fax
         adjx = ax + (cos da) * ((a.diam+b.diam + va*0.3)-dist)
         adjy = ay + (sin da) * ((a.diam+b.diam + va*0.3)-dist)
       in
         { a | speed <- va, dir <- da, pos <- (adjx, adjy)}
   in
     foldr bump ast colliding
     

---- Game state

------ New game

{-| Generate a new game -}
newGame n seed = 
  Playing 
    n
    {pos = (0, 0), dir = pi/2, speed = 0}
    (genAsteroids (n+5) seed)
    []
    []

{-| Generate a list of n asteroids in a circle -}
genAsteroids : Int -> Int -> [AstState]
genAsteroids n seed =
  let 
    {- Use own LCG based PRNG to generate an asteroid and pass the seed -}
    go n tot seed = 
      let
        seedN = (13 * seed + 654) % 701
        r = (2*pi) * (toFloat n / toFloat tot+1)
        d = toFloat (100 + (seedN % 149))
        pos = (d*cos r, d*sin r)
        dir = ((toFloat (seedN % 107))/107)*(2*pi)
        speed = toFloat (2 + (seedN % 3))
        diam = toFloat (10 + (seedN % 31))
        face = ((toFloat (seedN % 151))/151)*(2*pi)
        newAst = {pos = pos, dir = dir, speed = speed, diam = diam, face = face}
      in if | n == 0 -> []
            | otherwise -> newAst :: go (n-1) tot seedN
  in
    go n n seed


------ Step game

gameState : Signal GameState
gameState =
  foldp step Intro gameInput

step : GameInput -> GameState -> GameState
step (GameInput rnd key) state = 
  let
    nextGame n = newGame n rnd
    triggerNextGame n = if key.space then nextGame n else state
  in  
    case state of
      Intro  -> triggerNextGame 0
      Lost _ -> triggerNextGame 0
      Won  n -> triggerNextGame (n+1)
      Playing n ship asts bulls expls ->
        if | asts == []                     -> Won n
           | any (shipCollision ship) asts  -> Lost n
           | otherwise                      -> {- Update game with input -}
              let
                newBullet ship = {pos = ship.pos, dir = ship.dir, speed = ship.speed + 4}
                deleteBullet b = not ((outside b)||(astsBulletCollision asts b))
                bullsN = filter deleteBullet (if key.space 
                                                 then (newBullet ship) :: bulls
                                                 else bulls)
                deleteAst a = not (bulletsAstCollision bulls a)
                (astsN, explN) = partition deleteAst asts
              in 
                Playing 
                  n
                  (ship |> moveIt |> bounceIt |> controlIt key.arrows)
                  (map (rotateAst << moveIt << bounceIt << (astsAstCollision astsN)) astsN)
                  (map moveIt bullsN)
                  ((map explodeAst explN) ++ (dieExpls expls))

{-| Integrate position over speed -}
moveIt : Movable a -> Movable a
moveIt s = 
  let 
    x = fst(s.pos)
    y = snd(s.pos)
  in
    {s|pos <-(x + cos(s.dir) * s.speed, y + sin(s.dir) * s.speed)}

{-| Bounce against the walls -}
bounceIt : Movable a -> Movable a
bounceIt s =
  let 
    x = fst(s.pos)
    y = snd(s.pos)
    o = abs(s.speed)
  in
    if | x <  -250 -> { s | pos <- (x+o, y), dir <- pi - s.dir }
       | x >   250 -> { s | pos <- (x-o, y), dir <- pi - s.dir }
       | y <  -250 -> { s | pos <- (x, y+o), dir <- 0  - s.dir }
       | y >   250 -> { s | pos <- (x, y-o), dir <- 0  - s.dir }
       | otherwise -> s

{-| Steer the ship -}
controlIt : { x:Int, y:Int} -> ShipState -> ShipState
controlIt {x,y} s =
  { s | speed <- s.speed+(toFloat y)*0.2,
        dir <- s.dir - (toFloat x)*0.1}

{-| Has the bullet left space? -}
outside : BulletState -> Bool
outside s =
  let 
    x = fst(s.pos)
    y = snd(s.pos)
  in
    x < -250 || x > 250 || y < -250 || y > 250 

{-| Integrate asteroid rotation for a cool effect. The rotation is not physics based -}
rotateAst : AstState -> AstState
rotateAst a = 
  {a | face <- a.face + (cos a.dir)*(min 1 a.speed)*0.1}

{-| Asteroid dies and becomes an explostion particle -}
explodeAst : AstState -> AstExplode 
explodeAst a = 
  {pos = a.pos, diam = a.diam, face = a.face , life = 8}

{-| Update explosion particles -}
dieExpls : [AstExplode] -> [AstExplode]
dieExpls xs = 
  map (\a -> { a | life <- a.life-1}) (filter (\a -> a.life /= 0) xs)



-- INPUT

gameInput : Signal GameInput
gameInput = 
  sampleOn heartbeat (GameInput <~ randomWhenNeeded ~ keyInput)

heartbeat : Signal Float
heartbeat =
  fps 20
  
randomWhenNeeded : Signal Int
randomWhenNeeded = 
  Random.range 0 700 Keyboard.space

keyInput : Signal KeyInput
keyInput = 
  let
    clockedSpace = clocked (1*second) heartbeat Keyboard.space 
  in
    KeyInput <~ clockedSpace ~ Keyboard.arrows

{-| put a cooldown with autoretrigger on the bool signal. cooldown t based on time deltas. -}
clocked : Time -> Signal Time -> Signal Bool -> Signal Bool
clocked t sTd sB = 
  let
    step (inTd,inB) (stB,c) = if | c <= 0 && inB -> (True, t)
                                 | c <= 0 && not inB -> (False, 0)
                                 | c /= 0 -> (False, c-inTd)
  in
    fst <~ (foldp step (False, 0) (sampleOn sTd ((,) <~ sTd ~ sB)))



-- FULL GUI

main : Signal Element
main = 
  scene <~ gameState
