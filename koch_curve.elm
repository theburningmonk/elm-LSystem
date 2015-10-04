import Core exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List
import Dict
import Signal exposing (..)
import Window
import Debug

segmentLen = 15.0

kochCurve : LSystem
kochCurve =
  { axiom = [ 'F' ],
    rules = Dict.fromList [ 
              ('F', [ 'F', '+', 'F', '-', 'F', '-', 'F', '+', 'F' ])
            ] }

type alias Position = (Float, Float)
type alias Rotation = Float
type alias Length   = Float

display : (Int,Int) -> State -> Element
display (w,h) state = 
  let startPos = (-w/2, 0)
      content =
        [1..List.length states]
        |> List.map toFloat
        |> List.map (\n -> -rectHeight * n * 2 + toFloat h/3)
        -- above is to calc the y position for each state
        -- including whitespaces between states
        |> List.map2 (\state y -> drawState (toFloat w) y state) states 
        |> List.concat
  in collage w h (bg::content)

main = display <~ Window.dimensions ~ (states kochCurve)