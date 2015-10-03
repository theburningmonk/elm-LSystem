import Core exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Dict
import Signal exposing (..)
import String
import Window
import Debug

pTree : LSystem
pTree =
  { axiom = [ '0' ],
    rules = Dict.fromList [ 
              ('1', [ '1', '1' ]),
              ('0', [ '1', '[', '0', ']', '0' ])
            ] }

type alias Position = (Float, Float)
type alias Rotation = Float
type alias Length   = Float
type alias Stack    = List(Position, Rotation)

calcEndPos : Position -> Rotation -> Length -> Position
calcEndPos (x, y) rotation length =
    let endX = x + (length * cos rotation)
        endY = y + (length * sin rotation)
    in (endX, endY)

push : (Position, Rotation) -> Stack -> Stack
push x stack = x::stack

pop : Stack -> ((Position, Rotation), Stack)
pop (hd::tl) = (hd, tl)

createSegments : Position -> State -> List(Form)
createSegments pos state =
    let loop pos rotation length stack state acc =
        case state of
            [] -> acc
            '['::tl ->
                let newStack    = push (pos, rotation) stack
                    newRotation = rotation + pi/4
                in loop pos newRotation length newStack tl acc
            ']'::tl ->
                let ((newPos, newRotation'), newStack) = pop stack
                    newRotation = newRotation' - pi/4
                in loop newPos newRotation length newStack tl acc
            '0'::tl ->
                let endPos = calcEndPos pos rotation (length/2)
                    newSeg = segment pos endPos |> traced (solid black)
                in loop pos rotation length stack tl (newSeg::acc)
            '1'::tl ->
                let endPos = calcEndPos pos rotation length
                    newSeg = segment pos endPos |> traced (solid black)
                in loop endPos rotation length stack tl (newSeg::acc)

        rotation = pi/2 -- start with 90% (i.e. vertical)
        length   = 10   -- starting length
        stack    = []   -- start with empty stack (pos, rotation, length)
    in loop pos rotation length stack state []

display : (Int,Int) -> State -> Element
display (w,h) state = 
  let startPos = (0.0, -(toFloat h)/2) 
      paths    = createSegments startPos state
  in collage w h paths

main = display <~ Window.dimensions ~ (states pTree)