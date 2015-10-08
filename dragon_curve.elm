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

dragonCurve : LSystem
dragonCurve =
  { axiom = [ 'F', 'X' ],
    rules = Dict.fromList [ 
              ('X', String.toList "X+YF+"),
              ('Y', String.toList "-FX-Y")
            ] }

draw startPos state = 
  let angle = degrees 90
      (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            if | sym == 'F' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = segment pos endPos |> traced (solid black)
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
               | sym == '+' -> (pos, rotation+angle, acc, canvasArea)
               | sym == '-' -> (pos, rotation-angle, acc, canvasArea)
               | otherwise  -> (pos, rotation, acc, canvasArea)
          ) (startPos, 0, [], defaultCanvasArea)
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states dragonCurve)