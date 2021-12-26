module Program exposing (Inst(..), Proc, Program, parseProgram)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), float, int, lazy, loop, map, oneOf, sequence, spaces, succeed, token, variable)
import Set


{-| Instructions
-}
type Inst
    = Forward Float
    | Left Float
    | Right Float
      -- Repeat a list of instructions
    | Repeat Int Proc
      -- Call a procedure by its name
    | Call String


{-| Procedure aka list of instructions
-}
type alias Proc =
    List Inst


type alias Program =
    { procs : Dict String Proc, main : Proc }


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
        |= lazy (\_ -> pProc)


pIdentifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


pCall =
    succeed Call
        |. token "Call"
        |. spaces
        |= pIdentifier


pInst =
    oneOf [ pForward, pLeft, pRight, pRepeat, pCall ]


{-| Parser for a procedure, should be able to parse this :

    [ Forward 5, Right 3, Repeat 4 [ Forward 5, Call circle ] ]

-}
pProc =
    sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = pInst, trailing = Optional }


{-| Parser for a whole program, should be able to parse this :

    circle [ Repeat 12 [ Right 30, Forward 10 ] ]

    [ Repeat 6 [ Forward 10, Call circle ] ]

-}
pProgram =
    succeed Program
        |= loop Dict.empty
            (\procs ->
                oneOf
                    [ succeed (\identifier proc -> Loop (Dict.insert identifier proc procs))
                        |= pIdentifier
                        |. spaces
                        |= pProc
                    , succeed ()
                        |> map (\_ -> Done procs)
                    ]
            )
        |. spaces
        |= pProc


parseProgram text =
    Parser.run pProgram text
