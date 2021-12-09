module Main exposing (main)

import Browser
import Html exposing (Html, canvas, div, input, li, text, ul)
import Html.Attributes exposing (id, placeholder, value)
import Html.Events exposing (onInput)
import Parser
import Program exposing (Inst(..), Program, pProgram)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { progText : String }


init : Model
init =
    { progText = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newProgText ->
            { model | progText = newProgText }



-- VIEW


programList : Program -> Html Msg
programList prog =
    ul []
        (List.map
            (\inst ->
                case inst of
                    Repeat n subProg ->
                        li [] [ text ("Repeat " ++ String.fromInt n), programList subProg ]

                    other ->
                        li [] [ text (Debug.toString other) ]
            )
            prog
        )


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Program text", value model.progText, onInput Change ] []
        , case Parser.run pProgram model.progText of
            Ok prog ->
                programList prog

            -- TODO(guillaume) display parse errors
            Err err ->
                text "Error: "
        , canvas [ id "render" ] []
        ]
