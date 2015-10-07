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

sierpinskiTri : LSystem
sierpinskiTri =
  { axiom = [ 'A' ],
    rules = Dict.fromList [ 
              ('A', String.toList "+B-A-B+"),
              ('B', String.toList "-A+B+A-")
            ] }

draw startPos startRot state =
  let angle = degrees 60
      (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            if | sym == 'A' || sym == 'B' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = segment pos endPos |> traced (solid black)
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
               | sym == '+' -> (pos, rotation+angle, acc, canvasArea)
               | sym == '-' -> (pos, rotation-angle, acc, canvasArea)
          ) (startPos, startRot, [], defaultCanvasArea)
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states sierpinskiTri)