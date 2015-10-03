module Core where

import Dict exposing (..)
import Signal exposing (..)
import String
import Time

type alias Symbol  = Char
type alias State   = List(Symbol)
type alias Rules   = Dict Char State
type alias LSystem = { axiom : State, rules : Rules }

evolve : Rules -> State -> State
evolve rules state =
  state 
  |> List.map (\s -> get s rules |> Maybe.withDefault [s])
  |> List.concat
  
generations : Signal Int
generations = foldp (\_ n -> n + 1) 0 (Time.fps 1)

states : LSystem -> Signal State
states {axiom, rules} = 
  foldp (\_ state -> evolve rules state) axiom generations