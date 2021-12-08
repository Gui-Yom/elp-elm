module Main exposing (main)

import Html exposing (text)


type Inst
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Inst)


main =
    text "hi"
