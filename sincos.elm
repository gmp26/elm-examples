import Mouse
import Keyboard
import Window
import Graphics.Collage
import Graphics.Element
import String
import Text
import Debug

-- auxiliary functions, little helpers
tf n = toFloat n
cut n = String.left (if n >= 0 then 3 else 4) (show n)  -- format decimal places, sort of
cut2 n = String.left (if n >= 0 then 4 else 5) (show n)
cut3 n = String.left (if n >= 0 then 5 else 6) (show n)
signOfInfinity n m = if n >= 0 then (abs m) else -(abs m)  -- hack to display +/- infinity in a consistent way
r2d v = v * (180.0 / pi)
d2r v = v * (pi / 180)
make360 t = if t < 0 then (360-(-t)) else t
makeTau t = if t < 0 then (6.28-(-t)) else t
none = toForm empty
formatted col str = Text.color col <| toText str

initRotation = { theta = 0.0, nturns = 0 }

countTurns ((x,y),(w,h)) rotation = let 
                            offsetX = (tf w)/2
                            offsetY = (tf h)/2
                            currentX = (tf x) - offsetX
                            currentY = offsetY - (tf y)
                            polar = toPolar (currentX, currentY)
                            polar360 = (fst polar, makeTau <| snd polar)
                            angle360 = snd polar360
                            angleChange = (angle360) - (rotation.theta) -- new angle minus old angle
                            absAngleChange = abs angleChange
                            upOrDown = if angleChange < 0 then 1 else -1
                                    in 
                            {rotation | theta <- angle360,
                                        nturns <- (if absAngleChange > 3 
                                                  then (rotation.nturns + upOrDown) 
                                                  else rotation.nturns)
                                        }

input = let delta = lift (\t -> t/20) (fps 25)
        in sampleOn delta (lift2 (,) Mouse.position Window.dimensions)


main = lift4 (unitCircle) Mouse.position Window.dimensions (foldp countTurns initRotation input) (combine [count Mouse.clicks, count Keyboard.space, count Keyboard.ctrl, count Keyboard.shift])

unitCircle : (Int,Int) -> (Int,Int) -> { a | theta : Float, nturns : Int } -> [Int] -> Element
unitCircle (x,y) (w,h) turns [ mouseClick, kbdSpace, kbdCtrl, kbdShift ] = 
                let size = if (kbdSpace % 4 > 0) then 2 else 1 -- each keypress counts twice, on press and release
                    showTanLines = if (kbdCtrl % 4 > 0) then True else False
                    plotWaves = if (mouseClick % 2 == 0) then False else True
                    hideText = if (kbdShift % 4 > 0) then True else False
                    radius = tf (size * 100)
                    offsetX = ((tf w) / 2.0)
                    offsetY = ((tf h) / 2.0)
                    currentX = tf x - offsetX
                    currentY = offsetY - tf y
                    currentAngle = atan2 currentY currentX
                    currentDegree = r2d currentAngle
                    currentDegree360 = make360 currentDegree
                    currentDegreeRadians = d2r currentDegree360
                    dist = sqrt (currentX^2 + currentY^2)
                    distRatio = radius / dist
                    unitX = currentX * distRatio
                    unitY = currentY * distRatio
                    currentRadius = if dist < radius then radius else (tf (round dist))
                    onRadius = (unitX, unitY)
                    halfRadius = (unitX/2, unitY/2)
                    rpos = if dist < radius then halfRadius else ((currentX/2), (currentY/2))
                    xCorner = if dist < radius then (unitX,0) else (currentX, 0)
                    yCorner = if dist < radius then onRadius else (currentX,currentY)
                    currentTurns = if turns.nturns < 0 then turns.nturns + 1 else turns.nturns
                    hideTau = if (turns.nturns == 0 || turns.nturns == -1) then True else False
                    currentSign = if turns.nturns < 0 then -1 else 1
                    signedCurrentDegree360 = if turns.nturns < 0 then ((360 - currentDegree360)*currentSign) else (currentDegree360*currentSign)                    
                    signedCurrentDegreeRadians = if turns.nturns < 0 then ((6.28 - currentDegreeRadians)*currentSign) else (currentDegreeRadians*currentSign)
                    angle = move (0,-(radius*0.1)) <| toForm 
                                  <| leftAligned <| bold <| Text.color purple <| (toText "&nbsp;&nbsp;&#952;") ++ (toText " = ") 
                                     ++ (toText (cut2 <| if plotWaves then signedCurrentDegreeRadians else currentDegreeRadians))
                                     ++ (if (not hideTau) then ntau else (toText ""))
                    outerLimit = radius * 6.28
                    helvetica = ["helvetica","arial","sans-serif"]
                    {- X AND Y LABEL SETTINGS -}                    
                    labelRadius = radius * 1.6
                    labelDistRatio = labelRadius / dist
                    onLabelRadius = ((currentX * labelDistRatio), (currentY/2) * labelDistRatio)
                    yIndicator = outlined (dashed grey) <| path <| if | dist < radius -> [(0,unitY),onRadius]
                                            | otherwise -> [(0,currentY),(currentX,currentY)]                    
                    a = if dist < radius then (tf (round (unitX))/radius) 
                                         else (tf <| round currentX) / radius
                    b = if dist < radius then (tf (round (unitY))/radius) 
                                         else (tf <| round currentY) / radius
                    c = currentRadius / radius
                    xLabelPos = (currentX/2,-labelOffset)
                    xLabel = move xLabelPos <| alpha (if plotWaves then 0 else 1) <| toForm <| plainText <| "x=" ++ (cut a)
                    yLabelPos = if dist < labelRadius then onLabelRadius 
                                                      else (currentX,currentY/2)                    
                    yLabel = move yLabelPos <| alpha (if plotWaves then 0 else 1) <| toForm <| plainText <| "y=" ++ (cut b)
                    labelOffset = radius * 0.25                    
                    rLabel = move rpos <| alpha (if plotWaves then 0 else 1) <| toForm <| plainText <| "r=" ++ (cut c)
                    ntau = (toText (if plotWaves then 
                                          (if currentSign == 1 then " + " else " - ") 
                                          ++ (show <| abs currentTurns) ++ "&#964;" 
                                    else "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
                    {- FORMULAS -}
                    typeOfCircle = typeface helvetica <| Text.color grey <| toText (if (c == 1.0) then "Unit circle\n" else "\n")
                    angleAsDegrees = typeface helvetica <| (toText "angle = ") ++ (bold <| Text.color purple <| toText (cut2 <| d2r currentDegree360)) ++ (toText " radians &#8776; ")
                                                              ++ (toText (show <| round currentDegree360)) ++ (toText "&#176; &#8776; ")
                                                              ++ (toText (cut2 (currentDegreeRadians/(2*pi)))) ++ (toText "&#964;")
                    pythagoras = typeface helvetica <| (toText "\nradius = ") 
                                                        ++ (toText "&radic;(") ++ (toText <| cut a) ++ (toText "&#178; + ") 
                                                        ++ (toText <| cut b) ++ (toText "&#178;) &#8776; ") ++ (toText <| cut c)
                    currentDegreeDisplay = if (not plotWaves) then currentDegreeRadians else signedCurrentDegreeRadians
                    distanceAlongOuter = typeface helvetica <| (toText <| "\nDistance travelled along outer circle\n"
                                                         ++ "= r * &#952; = "
                                                         ++ (cut c)) ++ (toText " * ") 
                                                         ++ (bold <| Text.color purple <| toText (cut2 currentDegreeDisplay))
                                                         ++ (toText <| " &#8776; "
                                                         ++ (cut2 <| currentDegreeDisplay * c))
                    angleRadiusDistance = scale 0.8 <| move (-(tf w/4),(radius * -1.3)) <| toForm <| leftAligned 
                                          <| typeOfCircle ++ angleAsDegrees ++ pythagoras ++ distanceAlongOuter
                    sinx = b / c
                    cosx = a / c
                    tanx = sinx / cosx
                    sine = typeface helvetica <| formatted red <| "sin &#952; = y/r &#8776; " ++ (cut b) 
                                                                  ++ " / " ++ (cut c) 
                                                                  ++ " &#8776; " ++ (cut sinx)
                    cosine = typeface helvetica <| formatted blue <| "\ncos &#952; = x/r &#8776; " ++ (cut a) ++ " / " ++ (cut c) 
                                                                  ++ " &#8776; " ++ (cut cosx)
                    tangent = typeface helvetica <| (formatted orange "\ntan x = ") ++ (formatted red "sin x/") 
                                                                           ++ (formatted blue "cos x") 
                                                                           ++ (formatted orange <| " &#8776; " ++ cut2 sinx 
                                                                               ++ " / " ++ cut2 cosx 
                                                                               ++ " &#8776; " 
                                                                               ++ (if (abs tanx) < 10000.0 then (cut2 tanx) else (show (signOfInfinity currentY tanx))))
                    trig = scale 0.8 <| move ((tf w/4),(radius * -1.3)) <| toForm 
                                     <| rightAligned <| sine ++ cosine ++ tangent
                    sinIndicator = group [move (0,unitY) <| filled red <| circle (radius/20),
                                          outlined (thickerSine) <| path [(0,0),(0,unitY)]]
                    cosIndicator = group [move (if plotWaves then (0,unitX) else (unitX,0)) <| filled blue <| circle (radius/20),
                                          outlined (thickerCosine) <| path [(0,0),(unitX,0)]]
                    angleIndicator = group 
                                    [alpha (if plotWaves then 0.6 else 1) <| filled grey <| polygon <| 
                                     (0,0) :: map (\t -> fromPolar ( radius/5,degrees t)) 
                                     (if (currentSign == 1 || not plotWaves) then [0 .. currentDegree360] else [(360 - currentDegree360)*currentSign .. 0]),
                                     filled white <| circle (radius/6)]
                    xTicks = alpha (if plotWaves then 0 else 1)
                                   <| group <| map (\v -> alpha (if (abs v) < c then 1.0 else 0) 
                                   <| move (v*radius,0) <| filled blue <| circle (radius/80)) [-10 .. 10]
                    yTicks = group <| map (\v -> alpha (if (abs v) < c then 1.0 else 0) 
                                   <| move (0,v*radius) <| filled blue <| circle (radius/80)) [-10 .. 10]
                    {- POINT, TRIANGLE -}
                    innerCircle = outlined (solid blue) <| circle radius
                    outerCircle = alpha (if dist > radius then 0.7 else 0) 
                                   <| outlined (dashed blue) <| circle dist
                    p = move onRadius <| filled purple <| circle (radius/20)                    
                    freeTriangle = outlined (if plotWaves then (solid green) else thickerTriangle) <| path [(0,0),xCorner, yCorner] 
                    tangentPositionX = if | cosx > 0 -> radius 
                                          | cosx < 0 -> -radius
                                          | otherwise -> 0
                    tangentPositionY = tanx*radius
                    tangentPosition = (tangentPositionX,tangentPositionY)
                    tangentTriangle = if (showTanLines && tanx < 1000) then
                                    group [outlined (solid orange) <| path [(0,0),(tangentPositionX,0), tangentPosition],
                                    traced (thickerTanToY) <| path [tangentPosition,(0,tangentPositionY)],
                                    move (0,tangentPositionY) <| filled orange <| circle (radius/20) ]
                                    else none
                    waves = if plotWaves then (makeWaves (outerLimit) (currentDegreeRadians*radius) radius turns.nturns showTanLines) else none
                    cosineArrow = (if plotWaves then (traced (thickerCosineToY) <| path [(cosx*radius,0),(0,cosx*radius)]) else none)
                    {- STATIC ANNOTATION -}
                    hline = outlined (dotted grey) <| path [((if plotWaves then (-outerLimit) else -currentRadius),0),((if plotWaves then (outerLimit) else currentRadius),0)]
                    vline = outlined (dotted grey) <| path [(0,(if plotWaves then (-outerLimit) else -currentRadius)),(0,(if plotWaves then (outerLimit) else currentRadius))]
                    taus = alpha 1 <| group [move (radius * 1.2, 0) <| toForm <| plainText "0,&#964;",
                                        move (0, radius * 1.1) <| toForm <| plainText "1/4&#964;",
                                        move (radius * -1.2, 0) <| toForm <| plainText "1/2&#964;",
                                        move (0, radius * -1.1) <| toForm <| plainText "3/4&#964;"]
                    cartRange = alpha 1 <| group [move (radius * 1.1,0) <| toForm <| plainText "1",
                                       move (0,radius * 1.1) <| toForm <| plainText "1",
                                       move (radius * -1.1,0) <| toForm <| plainText "-1",
                                       move (0, radius * -1.1) <| toForm <| plainText "-1"]
                    circleAnnotation = if plotWaves then cartRange else taus
                    upperText = alpha (if (hideText || size == 2) then 0 else 1) <| scale 0.8 <| move (0,offsetY*0.7) <| toForm text1
                    lowerText = alpha (if (hideText || size == 2) then 0 else 1) <| scale 0.8 <| move (0,offsetY*(-0.7)) <| toForm text2                    
                        in
                        collage w h 
                               [innerCircle,
                                outerCircle,
                                rLabel, xLabel, yLabel,
                                angleIndicator,                                 
                                hline, vline,
                                xTicks, yTicks,
                                tangentTriangle,
                                freeTriangle,
                                yIndicator,
                                cosIndicator, sinIndicator,
                                angle, p,
                                circleAnnotation,
                                angleRadiusDistance,                                 
                                trig,                                
                                waves,
                                cosineArrow,
                                upperText, lowerText                                                               
                                ]


thickerSine = { defaultLine | width <- 3, color <- red }
thickerCosine = { defaultLine | width <- 3, color <- blue }
thickerCosineToY = { defaultLine | width <- 2, color <- blue, dashing <- [3,3] }
thickerTanToY = { defaultLine | width <- 2, color <- orange, dashing <- [3,3] }
thickerTriangle = { defaultLine | width <- 2, color <- green }
sineLine = { defaultLine | width <- 2, color <- red }
cosineLine = { defaultLine | width <- 2, color <- blue }
tanLine = { defaultLine | width <- 2, color <- orange }

text1 = [markdown|
# Trigonometry - a minimal introduction

A line is a set of points. A circle is a line where each "point" is at an equal distance to a center. 
We usually refer to the circle's center as the origin, and to the distance as the radius. 
The radius of a circle times a certain constant number equals its circumference. 
Let's call this number &#964; (Greek letter [tau](http://www.tauday.com/tau-manifesto), same as 2&#960;) to mean 
one &#964;urn, i e one complete counter-clockwise rotation around the origin (beginning at "3 o'clock"). 
One &#964; is approximately equal to 6.28 "[radians](http://en.wikipedia.org/wiki/Radians)". 
Radians are just another [angular unit](http://betterexplained.com/articles/intuitive-guide-to-angles-degrees-and-radians/), 
and so 6.28 radians is equivalent to 360 degrees. 
Any point on a plane can be represented in terms of its distance from the origin and a rotational angle. 
This system is called **polar coordinates**. 

A triangle is half a rectangle. You can put a triangle in between the origin and the outer edge of a circle. 
The longest side of the triangle (hypotenuse) equals the radius and 
can be derived from the two shorter sides with the Pythagorean theorem: 
r =  &radic;(x<sup>2</sup> + y<sup>2</sup>). 
In a "[unit circle](https://www.khanacademy.org/math/trigonometry/unit-circle-trig-func/Trig-unit-circle/v/unit-circle-definition-of-trig-functions-1)", 
which has a radius of 1, this formula will always sum to 1 
and the circumference will be equal to 1\*&#964;=&#964;. 

|]


text2 = [markdown|

A function is the mapping or relation between certain inputs and outputs. 
Using the length of the triangle's sides, you can derive the trigonometric functions sine and cosine. 
For any angle <font color="#75507b">**&#952; (theta)**</font>, these functions are abbreviated 
<font color="#ef2929">**sin &#952;**</font> and <font color="#729fcf">**cos &#952;**</font>. 
Dividing the height and width of the triangle with the radius/hypotenuse gives you the sine and cosine values respectively. 
<font color="#ef2929">**Sine**</font> and <font color="#729fcf">**cosine**</font> 
are only affected by the amount of rotation (<font color="#75507b">**&#952;**</font>) and not by the circle's size. 
In a unit circle the sine and cosine are always equal to the **x and y coordinates**. 

When plotted in regular **cartesian coordinates** 
(horizontal x, vertical y), sine and cosine look like waves. 
**Click the mouse** once to change the coordinate representation (sort of) and/or **press ctrl** to show the 
<font color="#f57900">tangent</font>. 
**Shift** toggles this text on and off while **space** amplifies the circle .
The basic forms of sine and cosine vary between -1 and 1, which means they have an "amplitude" of 1 (distance from the baseline).
The tangent function has no real amplitude, since its range is &#177;infinity.
The period (length of repeating pattern) is 1&#964; for sine and cosine, and half of that for the tangent. 
The amplitude and period are function parameters, and sin(x) is actually short for 1sin(1x). 
You can play with these parameters in plotting tools like
[Desmos](http://www.desmos.com) and 
[Grapher](http://computers.tutsplus.com/tutorials/getting-started-with-grapher-your-macs-built-in-graph-tool--mac-48440).

|]


makeWaves staticOffset dynamicOffset amp turnCount showTan = let
             aPeriod = amp * 6.28
             threePeriods = 3 * aPeriod 
             xDomain = [0 .. (round threePeriods)]
             wave = map (\n -> (tf n)/amp) <| filter (\n -> n % 5 == 0) xDomain 
             movingTicks = filter (\n -> n % (round (staticOffset/4)) == 0) xDomain
                    in
                              move (-dynamicOffset-staticOffset,0) <| group
                              [traced sineLine <| path <| (0,0) :: map (\n -> (n*amp, (sin n*amp))) wave,
                               traced cosineLine <| path <| (0,amp) :: map (\n -> (n*amp, (cos n*amp))) wave,
                               group <| map (\n -> move ((tf n),0) <| (filled black) <| circle (amp/40)) movingTicks,
                               group <| reverse <| map (\n -> move ((tf n),-(amp/4)) <| toForm <| plainText 
                                                         <| tauTicks turnCount (threePeriods, aPeriod) n) 
                                                         <| reverse (tail movingTicks),
                               if showTan then (traced tanLine <| path <| (0,amp) :: map (\n -> (n*amp, (tan n*amp))) wave) else (toForm empty) ]


tauTicks t (threeTurns, oneTurn) n = let
            threeQuarterTurn = oneTurn * 0.75
            halfTurn = oneTurn * 0.5
            quarterTurn = oneTurn * 0.25
            m = if | threeTurns / (tf n) < 1.5 -> 2
                   | threeTurns / (tf n) < 3 -> 1
                   | otherwise -> 0
            d = (t + m)  -- number of turns/periods to add to the label
            nPeriods = if | d == 0 -> "-"
                          | d == 1 -> ""
                          | d > 1 -> show (d-1)
                          | otherwise -> show (d)
            pluralS = (if (abs d) == 1 then "" else "s")
              in
                if d > 0 then
                  if | (%) n (round (oneTurn+(m*oneTurn))) == 0 -> if d == 0 then "0" else ((show (t+m)) ++ " &#964;urn" ++ pluralS)
                     | (%) n (round (threeQuarterTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 3/4&#964;"
                     | (%) n (round (halfTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 1/2&#964;"
                     | (%) n (round (quarterTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 1/4&#964;"
                     | otherwise -> ""
                else 
                  if | (%) n (round (oneTurn+(m*oneTurn))) == 0 -> if d == 0 then "0" else ((show (t+m)) ++ " &#964;urn" ++ pluralS)
                     | (%) n (round (threeQuarterTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 1/4&#964;"
                     | (%) n (round (halfTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 1/2&#964;"
                     | (%) n (round (quarterTurn+(m*oneTurn))) == 0 -> nPeriods ++ " 3/4&#964;"
                     | otherwise -> ""   

{- @er1kb -}