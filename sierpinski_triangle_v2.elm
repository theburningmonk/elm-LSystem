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
  { axiom = [ 'F', '-', 'G', '-', 'G' ],
    rules = Dict.fromList [ 
              ('F', String.toList "F-G+F+G-F"),
              ('G', String.toList "GG")
            ] }

draw startPos state =
  let angle = degrees -120
      (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            if | sym == 'F' || sym == 'G' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = segment pos endPos |> traced (solid black)
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
               | sym == '+' -> (pos, rotation+angle, acc, canvasArea)
               | sym == '-' -> (pos, rotation-angle, acc, canvasArea)
          ) (startPos, 0, [], defaultCanvasArea)
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states sierpinskiTri)