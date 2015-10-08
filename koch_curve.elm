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

kochCurve : LSystem
kochCurve =
  { axiom = [ 'F' ],
    rules = Dict.fromList [ 
              ('F', String.toList "F+F-F-F+F")
            ] }

draw startPos state =
  let (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            case sym of
              'F' -> 
                let endPos = calcEndPos pos rotation 10
                    newSeg = segment pos endPos |> traced (solid black)
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
              '+' -> (pos, rotation+pi/2, acc, canvasArea)
              '-' -> (pos, rotation-pi/2, acc, canvasArea)
          ) (startPos, 0, [], defaultCanvasArea)
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states kochCurve)