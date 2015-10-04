import Core exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List
import Dict
import Signal exposing (..)
import Window
import Debug

kochCurve : LSystem
kochCurve =
  { axiom = [ 'F' ],
    rules = Dict.fromList [ 
              ('F', [ 'F', '+', 'F', '-', 'F', '-', 'F', '+', 'F' ])
            ] }

type alias Position = (Float, Float)
type alias Rotation = Float
type alias Length   = Float
type alias CanvasArea = 
  {topL:Position, topR:Position, bottomL:Position, bottomR:Position}
defaultCanvasArea =
  {topL=(0,0), topR=(0,0), bottomL=(0,0), bottomR=(0,0)}

canvasDimension : CanvasArea -> (Float, Float)
canvasDimension area =
  let width  = (fst area.topR) - (fst area.topL)
      height = (snd area.topL) - (snd area.bottomL)
  in (width, height)

calcEndPos : Position -> Rotation -> Length -> Position
calcEndPos (x, y) rotation length =
  let endX = x + (length * cos rotation)
      endY = y + (length * sin rotation)
  in (endX, endY)

updateCanvasArea : CanvasArea -> Position -> CanvasArea
updateCanvasArea area pos =
  let f compareX compareY (xl, yl) (xr, yr) =
    (compareX xl xr, compareY yl yr)
  in { 
        topL = f min max area.topL pos,
        topR = f max max area.topR pos,
        bottomL = f min min area.bottomL pos,
        bottomR = f max min area.bottomR pos
     }

draw : Position -> Position -> Form
draw pos endPos = segment pos endPos |> traced (solid black)

display : (Int,Int) -> State -> Element
display (w,h) state = 
  let startPos = (0, 0)
      startRot = 0
      (_, _, segs, canvasArea) =
        state |> List.foldl (\sym (pos, rotation, acc, canvasArea) ->
            case sym of
              'F' -> 
                let endPos = calcEndPos pos rotation 10
                    newSeg = draw pos endPos
                    newAcc = newSeg::acc
                    newCanvasArea = updateCanvasArea canvasArea endPos
                in (endPos, rotation, newAcc, newCanvasArea)
              '+' -> (pos, rotation+pi/2, acc, canvasArea)
              '-' -> (pos, rotation-pi/2, acc, canvasArea)
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

main = display <~ Window.dimensions ~ (states kochCurve)