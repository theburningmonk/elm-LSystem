import Core exposing (..)
import Path exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List
import Dict
import Signal exposing (..)
import String
import Window
import Debug

type alias Position = Path.Position

angle = degrees 25

fractalPlant : LSystem
fractalPlant =
  { axiom = [ 'X' ],
    rules = Dict.fromList [ 
              ('X', String.toList "F−[[X]+X]+F[+FX]−X"),
              ('F', String.toList "FF")
            ] }

draw : Position -> Position -> Form
draw pos endPos = segment pos endPos |> traced (solid black)

display : (Int,Int) -> State -> Element
display (w,h) state = 
  let startPos = (0, 0)
      startRot = 0
      stackStack = []
      (_, _, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, stack, acc, canvasArea) ->
            if | sym == 'F' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = draw pos endPos
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, stack, newAcc, newCanvasArea)
               | sym == '+' -> (pos, rotation+angle, stack, acc, canvasArea)
               | sym == '-' -> (pos, rotation-angle, stack, acc, canvasArea)
               | sym == '[' -> 
                let newStack = push (pos, rotation) stack
                in (pos, rotation, newStack, acc, canvasArea)
               | sym == ']' ->
                let ((newPos, newRotation), newStack) = pop stack
                in (newPos, newRotation, newStack, acc, canvasArea)
               | otherwise  -> (pos, rotation, stack, acc, canvasArea)
          ) (startPos, startRot, stackStack, [], defaultCanvasArea)
      (canvasWidth, canvasHeight) = canvasDimension canvasArea
      _ = (canvasWidth, canvasHeight) |> Debug.watch "canvasDimension"
      _ = (w, h) |> Debug.watch "windowDimension"
      _ = canvasArea |> Debug.watch "canvas_area"
      scaleFactor = 
        if canvasWidth > (toFloat w) || canvasHeight > (toFloat h)
        then min (toFloat w / canvasWidth) (toFloat h / canvasHeight)
        else 1.0
      _ = scaleFactor |> Debug.watch "scaleFactor"
      (centreX, centreY) = canvasCentre canvasArea |> Debug.watch "canvas_area_centre"
      content = 
        group segs
        |> scale scaleFactor
        |> move (-canvasWidth*scaleFactor/2, -canvasHeight*scaleFactor/2)
  in (collage w h [content])

main = display <~ Window.dimensions ~ (states fractalPlant)