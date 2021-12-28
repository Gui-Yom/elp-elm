module Program exposing (Inst(..), Proc, Program, ProgramError(..), parseProgram)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Step, Trailing(..), float, int, lazy, loop, map, oneOf, sequence, spaces, succeed, token, variable)
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
    Dict String Proc


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


pInst : Parser Inst
pInst =
    oneOf [ pForward, pLeft, pRight, pRepeat, pCall ]


{-| Parser for a procedure, should be able to parse this :

    [ Forward 5, Right 3, Repeat 4 [ Forward 5, Call circle ] ]

-}
pProc : Parser Proc
pProc =
    sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = pInst, trailing = Optional }


{-| Parser for a whole program, should be able to parse this :

    circle [ Repeat 12 [ Right 30, Forward 10 ] ]

    [ Repeat 6 [ Forward 10, Call circle ] ]

-}
pProgram : Parser Program
pProgram =
    succeed identity
        |. spaces
        |= loop Dict.empty
            (\procs ->
                oneOf
                    [ succeed (\identifier proc -> P.Loop (Dict.insert identifier proc procs))
                        |= pIdentifier
                        |. spaces
                        |= pProc
                        |. spaces
                    , succeed (\proc -> P.Loop (Dict.insert "main" proc procs))
                        |= pProc
                    , succeed ()
                        |> map (\_ -> P.Done procs)
                    ]
            )



-- TODO(guillaume) use advanced parser (Parser.Advanced) to check for errors while parsing


type ProgramError
    = SyntaxError DeadEnd
    | UnknownProc String
    | Loop String
    | NoMain


{-| Check the program for errors other than syntax errors
-}
checkProgram : Program -> List ProgramError
checkProgram prog =
    -- Check for main procedure existence
    (if Dict.member "main" prog then
        []

     else
        [ NoMain ]
    )
        -- Check for errors with instructions
        ++ Dict.foldl
            (\name proc errors ->
                errors
                    ++ List.foldl
                        (\inst procErrors ->
                            procErrors
                                ++ (case inst of
                                        Call callee ->
                                            -- Check the call refer to an existing procedure
                                            if Dict.member callee prog then
                                                -- Check for a simple loop
                                                if callee == name then
                                                    [ Loop callee ]

                                                else
                                                    []

                                            else
                                                [ UnknownProc callee ]

                                        _ ->
                                            []
                                   )
                        )
                        []
                        proc
            )
            []
            prog


parseProgram : String -> ( Maybe Program, List ProgramError )
parseProgram text =
    case P.run pProgram text of
        Err err ->
            ( Nothing, List.map (\e -> SyntaxError e) err )

        Ok prog ->
            ( Just prog, checkProgram prog )
