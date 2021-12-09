module Program exposing (Inst(..), Program, pProgram)

import Parser exposing ((|.), (|=), Parser, Trailing(..), int, lazy, oneOf, sequence, spaces, succeed, token)


type Inst
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Inst)


type alias Program =
    List Inst


pForward =
    succeed Forward
        |. token "Forward"
        |. spaces
        |= int


pLeft =
    succeed Left
        |. token "Left"
        |. spaces
        |= int


pRight =
    succeed Right
        |. token "Right"
        |. spaces
        |= int


pRepeat =
    succeed Repeat
        |. token "Repeat"
        |. spaces
        |= int
        |. spaces
        |= lazy (\_ -> pProgram)


pInst =
    oneOf [ pForward, pLeft, pRight, pRepeat ]


pProgram =
    sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = pInst, trailing = Optional }
