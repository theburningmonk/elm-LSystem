import Core exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List
import Dict
import Signal exposing (..)
import Window
import Debug

rectHeight = 25.0

cantorDust : LSystem
cantorDust =
  { axiom = [ 'A' ],
    rules = Dict.fromList [ 
              ('A', [ 'A', 'B', 'A' ]),
              ('B', [ 'B', 'B', 'B' ])
            ] }

drawState : Float -> Float -> State -> List(Form)
drawState winWidth y state =
  let len = List.length state |> toFloat |> Debug.watch "count"
      rectWidth = winWidth / len
      startX = -winWidth/2 + rectWidth/2
      xs = [0..(List.length state-1)] 
           |> List.map toFloat
           |> List.map (\n -> startX + n*rectWidth)
      rects =
        List.map2 (\sym x -> 
          let colour = 
            case sym of
              'A' -> black
              'B' -> white
          in rect rectWidth rectHeight
             |> filled colour
             |> move (x, y)
        ) state xs
  in rects      

display : (Int,Int) -> List(State) -> Element
display (w,h) states = 
  let bg = rect (toFloat w) (toFloat h) |> filled white
      content =
        [1..List.length states]
        |> List.map toFloat
        |> List.map (\n -> -rectHeight * n * 2 + toFloat h/3)
        -- above is to calc the y position for each state
        -- including whitespaces between states
        |> List.map2 (\state y -> drawState (toFloat w) y state) states 
        |> List.concat
  in collage w h (bg::content)

allStates : Signal (List(State))
allStates = 
  let {axiom, rules} = cantorDust
      states = foldp (\_ (hd::tl) -> (evolve rules hd)::hd::tl) 
                     [axiom] 
                     generations
  in List.reverse <~ states

main = display <~ Window.dimensions ~ allStates