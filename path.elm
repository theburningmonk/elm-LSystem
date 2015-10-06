module Path where

type alias Position = (Float, Float)
type alias Rotation = Float
type alias Length   = Float

calcEndPos : Position -> Rotation -> Length -> Position
calcEndPos (x, y) rotation length =
  let endX = x + (length * cos rotation)
      endY = y + (length * sin rotation)
  in (endX, endY)

type alias Stack = List(Position, Rotation)

push : (Position, Rotation) -> Stack -> Stack
push x stack = x::stack

pop : Stack -> ((Position, Rotation), Stack)
pop (hd::tl) = (hd, tl)

type alias CanvasArea = 
  {topL:Position, topR:Position, bottomL:Position, bottomR:Position}

defaultCanvasArea =
  {topL=(0,0), topR=(0,0), bottomL=(0,0), bottomR=(0,0)}

canvasDimension : CanvasArea -> (Float, Float)
canvasDimension area =
  let width  = (fst area.topR) - (fst area.topL)
      height = (snd area.topL) - (snd area.bottomL)
  in (width, height)

canvasCentre : CanvasArea -> (Float, Float)
canvasCentre {bottomL, bottomR, topL, topR} =
  let centreX = (fst topL + fst topR)/2
      centreY = (snd topL + snd bottomL)/2
  in (centreX, centreY)

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