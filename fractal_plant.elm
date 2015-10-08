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
              ('X', String.toList "F−[[X]+X]+F[+FX]−X"),
              ('F', String.toList "FF")
            ] }

draw startPos startRot state =
  let angle = degrees 25
      startStack = []
      (_, _, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, stack, acc, canvasArea) ->
            if | sym == 'F' ->
                let endPos = calcEndPos pos rotation 10
                    newSeg = segment pos endPos |> traced (solid black)
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
          ) (startPos, startRot, startStack, [], defaultCanvasArea)      
  in (segs, canvasArea)

main = (display draw) <~ Window.dimensions ~ (states fractalPlant)