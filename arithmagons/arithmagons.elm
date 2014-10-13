import Graphics.Input (Input, input)
import Graphics.Input.Field as Field
import Text

content : Input Field.Content
content = input Field.noContent


tf = toFloat

render : State -> (Int,Int)  -> Element
render state (w,h) = 
    let rGon = 2*(tf w) / 5
        rVert = (tf w) / 10
    in [ngon 3 rGon
            |> outlined {lineStyle | width <- 3}
            |> rotate (-pi/6)
        , vertex rVert |> move (rGon*cos (-pi/6), rGon*sin (-pi/6))
        , vertex rVert |> move (rGon*cos (pi/2), rGon*sin (pi/2))
        , vertex rVert |> move (rGon*cos (7*pi/6), rGon*sin (7*pi/6))
        ]
                |> collage w h 

lineStyle = solid black

vertex : Float -> Form
vertex radius = 
    [ circle radius
        |> filled red 
    , circle radius
        |> outlined (solid black)
    , toForm (inputField 40 40)    
    ]
    |> group

inputField w h = width w <| height h <| (Field.field fieldStyle content.handle identity "Type here!" Field.noContent)

type State = {}
state = {}

ds = Field.defaultStyle
fieldStyle =  { ds | outline <- 
                      { color = red
                      , width = Field.uniformly 1
                      , radius = 5
                      }     
                    , style <-
                        { typeface = [ "Times New Roman", "serif" ]
  , height   = Just 20
  , color    = blue
  , bold     = False
  , italic   = False
  , line     = Nothing
  }
                     
              } 


main : Element

main = render state (400,400)
