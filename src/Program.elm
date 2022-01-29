module Program exposing (Inst(..), Proc, Program, ProgramError(..), parseProgram)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Step, Trailing(..), chompIf, chompWhile, float, getChompedString, int, keyword, lazy, loop, map, oneOf, sequence, spaces, succeed, symbol, variable)
import Set exposing (Set)


{-| Instructions
-}
type Inst
    = -- Advance forward using all defined params
      Forward Float
      -- Turn left in degrees
    | Left Float
      -- Turn right in degrees
    | Right Float
      -- Repeat an instruction
    | Repeat Int Proc
      -- Call a procedure by its name
    | Call String
      -- Change stroke color
    | Color String
      -- Change stroke width
    | Width Float
      -- Save stack
    | Scope Proc


{-| Procedure aka list of instructions
-}
type alias Proc =
    List Inst


{-| A Program is a set of named procedures
-}
type alias Program =
    Dict String Proc


pForward =
    succeed Forward
        |. keyword "Forward"
        |. spaces
        |= float


pLeft =
    succeed Left
        |. keyword "Left"
        |. spaces
        |= float


pRight =
    succeed Right
        |. keyword "Right"
        |. spaces
        |= float


pRepeat =
    succeed Repeat
        |. keyword "Repeat"
        |. spaces
        |= int
        |. spaces
        |= pInstOrProc


pIdentifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


pCall =
    succeed Call
        |. keyword "Call"
        |. spaces
        |= pIdentifier


{-| Should parse :

    #11FADB

-}
pColorHex =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c == '#')
            |. chompWhile Char.isHexDigit


{-| Should parse :

    rgb ( 124, 11, 48 )

-}
pColorFunc func =
    succeed (\r g b -> func ++ "(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")")
        |. symbol (func ++ "(")
        |. spaces
        |= int
        |. spaces
        |. symbol ","
        |. spaces
        |= int
        |. spaces
        |. symbol ","
        |. spaces
        |= int
        |. spaces
        |. symbol ")"


pColorInst =
    succeed Color
        |. keyword "Color"
        |. spaces
        -- A color is either in hex notation (#DEADFE), function notation (rgb(r, g, b), hsl(h, s, l)) or a css color name
        |= oneOf [ pColorHex, pColorFunc "rgb", pColorFunc "hsl", pIdentifier ]


pWidth =
    succeed Width
        |. keyword "Width"
        |. spaces
        |= float


pScope =
    succeed Scope
        |= lazy (\_ -> pProc)


{-| Parse an instruction
-}
pInst : Parser Inst
pInst =
    oneOf [ pForward, pLeft, pRight, pRepeat, pCall, pColorInst, pWidth, pScope ]


{-| Parser for a procedure, should be able to parse this :

    [ Forward 5, Right 3, Repeat 4 [ Forward 5, Call circle ] ]

-}
pProc : Parser Proc
pProc =
    sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = lazy (\_ -> pInst), trailing = Optional }


pInstAsProc =
    succeed (\inst -> [ inst ])
        |= lazy (\_ -> pInst)


pInstOrProc =
    oneOf [ pProc, pInstAsProc ]


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
                    -- Named procedure
                    [ succeed (\identifier proc -> P.Loop (Dict.insert identifier proc procs))
                        |= pIdentifier
                        |. spaces
                        |= pProc
                        |. spaces

                    -- Main proc can be unnamed
                    , succeed (\proc -> P.Loop (Dict.insert "main" proc procs))
                        |= pProc
                    , succeed ()
                        |> map (\_ -> P.Done procs)
                    ]
            )



-- TODO(guillaume) use advanced parser (Parser.Advanced) to check for errors while parsing


type ProgramError
    = SyntaxError DeadEnd -- Parse error
    | UndefinedProc String -- Call to undefined procedure
    | Loop (Set String) -- Call graph loop
    | NoMain -- No main procedure defined


{-| Create a list of the calls made from this instruction
-}
callGraphInst : Inst -> List String
callGraphInst inst =
    case inst of
        Call callee ->
            [ callee ]

        Repeat _ proc ->
            callGraphProc proc

        Scope proc ->
            callGraphProc proc

        _ ->
            []


{-| Create a list of the calls made from this procedure
-}
callGraphProc : Proc -> List String
callGraphProc proc =
    List.concatMap (\inst -> callGraphInst inst) proc


{-| Create a call graph of a program as a dict
-}
callGraph : Program -> Dict String (List String)
callGraph prog =
    Dict.map (\k v -> callGraphProc v) prog


{-| Traverse the call graph searching for loops and invalid calls
-}
traverseCallGraph : Dict String (List String) -> Set String -> String -> List ProgramError
traverseCallGraph graph visited next =
    if Set.member next visited then
        [ Loop visited ]

    else
        let
            children =
                Dict.get next graph
        in
        case children of
            Just nexts ->
                List.concatMap (\n -> traverseCallGraph graph (Set.insert next visited) n) nexts

            Nothing ->
                [ UndefinedProc next ]


{-| Driver function for traverseCallGraph
-}
checkCallGraph : Program -> List ProgramError
checkCallGraph prog =
    traverseCallGraph (callGraph prog) Set.empty "main"


{-| Check the program for errors other than syntax errors
-}
checkProgram : Program -> List ProgramError
checkProgram prog =
    -- Check for main procedure existence
    if Dict.member "main" prog then
        checkCallGraph prog

    else
        [ NoMain ]


{-| Parse a program from input and check for errors
-}
parseProgram : String -> ( Maybe Program, List ProgramError )
parseProgram text =
    case P.run pProgram text of
        Err err ->
            ( Nothing, List.map (\e -> SyntaxError e) err )

        Ok prog ->
            ( Just prog, checkProgram prog )
