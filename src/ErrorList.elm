module ErrorList exposing (view)

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (id)
import Program exposing (ProgramError(..))
import Set


{-| Display program errors that prevent it from running
-}
view : List ProgramError -> Html msg
view errors =
    if List.isEmpty errors then
        div [] []

    else
        ul [ id "errorList" ]
            (List.map
                (\e ->
                    li []
                        [ text
                            (case e of
                                SyntaxError d ->
                                    "Syntax error : " ++ Debug.toString d.problem ++ " at (" ++ String.fromInt d.row ++ ", " ++ String.fromInt d.col ++ ")"

                                UndefinedProc proc ->
                                    "Undefined procedure : " ++ proc

                                Loop cycle ->
                                    "Infinite loop : " ++ Debug.toString (Set.toList cycle)

                                NoMain ->
                                    "No main procedure"
                            )
                        ]
                )
                errors
            )
