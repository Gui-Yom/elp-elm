module ErrorList exposing (view)

import Html exposing (Html, li, text, ul)
import Html.Attributes exposing (id)
import Program exposing (ProgramError(..))


{-| Display program errors that prevent it from running
-}
view : List ProgramError -> Html msg
view errors =
    ul [ id "errorList" ]
        (List.map
            (\e ->
                li []
                    [ text
                        (case e of
                            SyntaxError d ->
                                "Syntax error : " ++ Debug.toString d.problem ++ " at (" ++ String.fromInt d.row ++ ", " ++ String.fromInt d.col ++ ")"

                            UnknownProc proc ->
                                "Unknown procedure name : " ++ proc

                            Loop proc ->
                                "Infinite loop in procedure : " ++ proc

                            NoMain ->
                                "No main procedure"
                        )
                    ]
            )
            errors
        )
