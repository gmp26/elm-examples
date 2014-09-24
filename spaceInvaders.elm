import Keyboard
import Window
import Debug

(gameWidth,gameHeight) = (600,800)
(halfWidth,halfHeight) = (300,400)


type Input = {space:Bool, dir:Int, delta:Time, pulse:Time}
type Spaceship = {x:Float, y:Float, rotation:Float}
type Ball = {x:Float, y:Float, vx:Float, vy:Float, angle:Float, status:FlyingElementState}
type Enemy = {x:Float, y:Float, vx:Float, vy:Float, status:FlyingElementState}
type Game = {spaceship:Spaceship, balls:[Ball], enemies:[Enemy], state:State, lastPulse:Maybe Time, isGameOver:Bool}

data State = Shooting | NotShooting
data FlyingElementState = Flying | OutOfBounds | Colliding | ReadyToFly


positionEnemies : [Float]
positionEnemies = [-290, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 290]


-- default elements
defaultBall : Float -> Ball
defaultBall a = {x=0, y=-halfHeight+40, vx=600, vy=600, angle=a, status=Flying}


defaultSpaceship : Float -> Spaceship
defaultSpaceship h = {x=0, y=h + 40, rotation=90}

defaultEnemy : Int -> Time -> Enemy
defaultEnemy index lastPulse = let indexToSelect =  if (index == 0) then (((truncate lastPulse) + 10) % (length positionEnemies)) else  (index % (length positionEnemies))
                               in {x=(getXEnemy indexToSelect positionEnemies), y=halfHeight-40, vx=200, vy=200, status=Flying}

defaultGame : Game
defaultGame = 
  { spaceship = {x=0, y=-halfHeight+40, rotation=90},
    balls = [],
    enemies = [defaultEnemy 3 0],
    state = NotShooting,
    lastPulse = Nothing,
    isGameOver = False
  }




-- signals 

delta = inSeconds <~ fps 35
pulse = every (0.5 * second)  
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.arrows
                               ~ delta
                               ~ pulse)
                               

-- update

stepGame : Input -> Game -> Game
stepGame ({space, dir, delta, pulse} as input) ({spaceship, balls, enemies, state, lastPulse, isGameOver} as game) =
      let spaceship' = moveSpaceship spaceship dir
          balls' = addBall balls space delta spaceship.rotation state enemies
          enemies' = addEnemy enemies balls delta pulse lastPulse
          state' = updateState state space
          lastPulse' = updateLastPulse lastPulse pulse
          isGameOver' = checkIfGameOver enemies
      in { game | spaceship  <- if isGameOver then spaceship else spaceship',
                  balls      <- if isGameOver then balls else balls',
                  enemies    <- if isGameOver then enemies else enemies',
                  state      <- if isGameOver then state else state',
                  lastPulse  <- lastPulse',
                  isGameOver <- isGameOver'
                }



-- helpers

checkIfGameOver : [Enemy] -> Bool
checkIfGameOver enemies = any (\e -> e.status == OutOfBounds) enemies

getXEnemy : Int -> [Float] -> Float
getXEnemy index indexes = case index
                         of 0 -> head indexes
                            1 -> head indexes
                            _ -> last (take index indexes)

createEnemy : Float -> Enemy
createEnemy posX = {x=posX, y=halfHeight+10, vx=200, vy=200, status=Flying}

createEnemies : [Float] -> [Enemy]
createEnemies positions = map (\px -> createEnemy px) positions



addBall : [Ball] -> Bool -> Time -> Float -> State -> [Enemy] -> [Ball]
addBall balls isSpacePressed delta angle state enemies =  let balls' = if isSpacePressed then balls ++ [defaultBall angle] else balls
                                                        in updateAllBalls balls' delta angle state enemies


addEnemy : [Enemy] -> [Ball] -> Time -> Time -> Maybe Time -> [Enemy]
addEnemy enemies balls delta pulse lastPulse = let enemies' = case (pulse, lastPulse)
                                                             of (_, Nothing) -> enemies
                                                                (p, Just lp) -> if (p /= lp) then enemies ++ [(defaultEnemy (length balls) lp)] else enemies
                                                in updateAllEnemies enemies' balls delta


updateLastPulse : Maybe Time -> Time -> Maybe Time
updateLastPulse lastPulse newPulse = case (lastPulse, newPulse)
                                     of (Nothing, np) -> Just newPulse
                                        (Just lp, np) -> if lp /= np then Just np else lastPulse


updateAllEnemies : [Enemy] -> [Ball] -> Time -> [Enemy]
updateAllEnemies enemies balls delta = filter (\e -> e.status /= Colliding) (map (\e -> updateEnemy e balls delta) enemies)


updateEnemy : Enemy -> [Ball] -> Time -> Enemy
updateEnemy enemy balls delta = let y' = updateYEnemy enemy delta
                                    state' = updateStatusEnemy enemy balls
                                in {enemy | y <- y', status <- state'}


updateYEnemy : Enemy -> Time -> Float
updateYEnemy enemy delta = enemy.y - enemy.vy * delta


updateStatusEnemy : Enemy -> [Ball] -> FlyingElementState
updateStatusEnemy enemy balls = if checkCollisions enemy balls then Colliding 
                               else if enemy.y <= -halfHeight then OutOfBounds 
                               else Flying



updateAllBalls : [Ball] -> Time -> Float -> State -> [Enemy] -> [Ball]
updateAllBalls balls delta angle state enemies = filter (\b -> b.status /= Colliding && b.status /= OutOfBounds) (map (\b -> moveBall b delta state angle (checkCollisionsWithEnemies b enemies)) balls)


updateState : State -> Bool -> State
updateState state isSpacePressed = case (state, isSpacePressed) of
                                   (Shooting, _)        -> Shooting
                                   (NotShooting, True)  -> Shooting
                                   (NotShooting, False) -> NotShooting
                                   (_,_)                -> NotShooting


updateBallState : Ball -> Bool -> FlyingElementState 
updateBallState ball isColliding = if isColliding then Colliding
                                   else if ball.y > halfHeight || ball.x < -halfWidth || ball.x > halfWidth then OutOfBounds
                                   else if ball.status == ReadyToFly then Flying
                                   else Flying


updateBallPosition : Ball -> State -> Time -> (Float,Float)
updateBallPosition ({x,y,vx,vy,angle,status} as ball) state delta= case (x, y, status, state) of
                                                                   (_,_,ReadyToFly, Shooting) -> (0, -halfHeight+40)
                                                                   (_,_,Flying,NotShooting)   -> (0, -halfHeight+40)
                                                                   (_,_,Flying,Shooting)      -> (x + vx * delta * cos (convertDegreesToRadian angle), y + vy * delta * sin (convertDegreesToRadian ball.angle))



moveBall : Ball -> Time -> State -> Float -> Bool -> Ball
moveBall ({x,y,vx,vy} as ball) delta state angle isColliding = let (x',y') = updateBallPosition ball state delta
                                                                   status' = updateBallState ball isColliding
                                                                   angle' = if state == NotShooting || ball.status == ReadyToFly then angle else ball.angle
                                                               in {ball | x <- x', y <- y', angle <- angle', status <- status' }

convertDegreesToRadian : Float -> Float
convertDegreesToRadian angleInDegree = angleInDegree / 180 * pi

moveSpaceship : Spaceship -> Int -> Spaceship
moveSpaceship spaceship angle = let rotation' = spaceship.rotation - ((toFloat angle) * 5)
                        in {spaceship | rotation <- rotation'}

moveEnemy : Enemy -> Time -> [Ball] -> Enemy
moveEnemy ({x,y,vx,vy} as enemy) delta balls = let y' = if (y > -halfHeight && y < halfHeight && not (checkCollisions enemy balls))
                                                        then y - vy * delta 
                                                        else halfHeight-40 
                        in {enemy | y <- y'}
                 
checkCollisions : Enemy -> [Ball] -> Bool
checkCollisions enemy balls = any (\b -> isColliding enemy b) balls 

checkCollisionsWithEnemies : Ball -> [Enemy] -> Bool
checkCollisionsWithEnemies b enemies = any (\e -> isColliding e b) enemies
                       
isColliding : Enemy -> Ball -> Bool                       
isColliding enemy ball = (abs (enemy.x - ball.x)) < 30 && (abs (enemy.y - ball.y)) < 30


display : (Int, Int) -> Game -> Input -> Element
display (w,h) {spaceship, balls, enemies, state, lastPulse, isGameOver} i  = collage w h [
                move (0, 0) (filled yellow (rect gameWidth gameHeight)),
                (drawSpaceship spaceship red),
                (displayBalls balls),
                (drawEnemies enemies),
                (displayGameOverOverlay isGameOver w h)
                ]

displayBalls : [Ball] -> Form
displayBalls balls = group (map (\b -> drawBall b blue) balls)

drawEnemies : [Enemy] -> Form
drawEnemies enemies = group (map (\b -> drawEnemy b brown) enemies)

drawSpaceship : Spaceship -> Color -> Form
drawSpaceship spaceship clr = (rotate (degrees spaceship.rotation) ( move (spaceship.x, spaceship.y) (filled clr (ngon 3 20))))

drawEnemy : Enemy -> Color -> Form
drawEnemy enemy clr = move (enemy.x, enemy.y) (filled clr (ngon 4 15))

drawBall : Ball -> Color -> Form
drawBall ball clr = ( move (ball.x, ball.y) (filled clr (circle 4)))

displayGameOverOverlay : Bool -> Int -> Int -> Form
displayGameOverOverlay isGameOver w h = let w' = if isGameOver then w else 0
                                            h' = if isGameOver then h else 0
                                            text' = if isGameOver then "GAME OVER" else ""
                                        in group [filled (rgba 255 255 255 0.8) (rect (toFloat w') (toFloat h')), 
                                                  move (0,0) (toForm (plainText text'))]

-- entry point

gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = lift3 display Window.dimensions gameState input
