module Core where

import Dict exposing (..)
import Signal exposing (..)
import String
import Time
import Keyboard
import Debug

type alias Symbol  = Char
type alias State   = List(Symbol)
type alias Rules   = Dict Char State
type alias LSystem = { axiom : State, rules : Rules }

evolve : Rules -> State -> State
evolve rules state =
  state 
  |> List.map (\s -> get s rules |> Maybe.withDefault [s])
  |> List.concat

evolveTo : LSystem -> Int -> State
evolveTo {axiom, rules} gen =
  let loop n state =
        if n >= gen then state
        else evolve rules state |> loop (n+1)
  in loop 0 axiom

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

generations : Signal Int
generations = 
  foldp (\x n -> n + x |> max 0) 0 genChanges

states : LSystem -> Signal State
states lSys = 
  (evolveTo lSys) <~ (Debug.watch "gen" <~ generations)