module Program exposing (Inst(..), Program, pProgram)

import Parser exposing ((|.), (|=), Parser, Trailing(..), float, int, lazy, oneOf, sequence, spaces, succeed, token)


type Inst
    = Forward Float
    | Left Float
    | Right Float
    | Repeat Int (List Inst)


type alias Program =
    List Inst


pForward =
    succeed Forward
        |. token "Forward"
        |. spaces
        |= float


pLeft =
    succeed Left
        |. token "Left"
        |. spaces
        |= float


pRight =
    succeed Right
        |. token "Right"
        |. spaces
        |= float


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
