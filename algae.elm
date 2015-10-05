import Core exposing (..)

import Graphics.Element exposing (..)
import Dict
import Signal exposing (..)
import String

algae : LSystem
algae =
  { axiom = [ 'A' ],
    rules = Dict.fromList [ 
              ('A', [ 'A', 'B' ]),
              ('B', [ 'A' ])
            ] }        

output = String.fromList <~ (states algae)
main = show <~ output