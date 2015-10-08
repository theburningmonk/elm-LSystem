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

fractalPlant : LSystem
fractalPlant =
  { axiom = [ 'X' ],
    rules = Dict.fromList [
              ('X', String.toList "F-[[X]+X]+F[+FX]-X"),
              ('F', String.toList "FF")
            ] }

draw startPos startRot state =
  let angle = degrees 25
      startStack = []
      (_, _, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rot, stack, acc, canvasArea) ->
            case sym of
              'F' ->
                let endPos = calcEndPos pos rot 10
                    newSeg = segment pos endPos |> traced (solid black)
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rot, stack, newAcc, newCanvasArea)
              '+' -> (pos, rot+angle, stack, acc, canvasArea)
              '-' -> (pos, rot-angle, stack, acc, canvasArea)
              '[' ->
                let newStack = push (pos, rot) stack
                in (pos, rot, newStack, acc, canvasArea)
              ']' ->
                let ((newPos, newRot), newStack) = pop stack
                in (newPos, newRot, newStack, acc, canvasArea)
              _ -> (pos, rot, stack, acc, canvasArea)
          ) (startPos, startRot, startStack, [], defaultCanvasArea)
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states fractalPlant)