import Core exposing (..)
import Path exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List
import Dict
import Signal exposing (..)
import Window
import Debug

type alias Position = Path.Position

angle = pi/3 -- 60 degrees

sierpinskiTri : LSystem
sierpinskiTri =
  { axiom = [ 'A' ],
    rules = Dict.fromList [ 
              ('A', [ '+', 'B', '-', 'A', '-', 'B', '+' ]),
              ('B', [ '-', 'A', '+', 'B', '+', 'A', '-' ])
            ] }

draw : Position -> Position -> Form
draw pos endPos = segment pos endPos |> traced (solid black)

display : (Int,Int) -> State -> Element
display (w,h) state = 
  let startPos = (0, 0)
      startRot = 0
      (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            if | sym == 'A' || sym == 'B' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = draw pos endPos
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
               | sym == '+' -> (pos, rotation+angle, acc, canvasArea)
               | sym == '-' -> (pos, rotation-angle, acc, canvasArea)
          ) (startPos, startRot, [], defaultCanvasArea)
      (canvasWidth, canvasHeight) = canvasDimension canvasArea
      _ = (canvasWidth, canvasHeight) |> Debug.watch "canvasDimension"
      _ = (w, h) |> Debug.watch "windowDimension"
      _ = canvasArea |> Debug.watch "canvas_area"
      scaleFactor = 
        if canvasWidth > (toFloat w) || canvasHeight > (toFloat h)
        then min (toFloat w / canvasWidth) (toFloat h / canvasHeight)
        else 1.0
      _ = scaleFactor |> Debug.watch "scaleFactor"
      content = 
        group segs
        |> scale scaleFactor
        |> move (-canvasWidth*scaleFactor/2, -canvasHeight*scaleFactor/2)        
  in (collage w h [content])

main = display <~ Window.dimensions ~ (states sierpinskiTri)