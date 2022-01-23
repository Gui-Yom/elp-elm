module Main exposing (main)

import Browser
import Canvas
import ErrorList
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (autofocus, class, cols, id, placeholder, rows, spellcheck, value)
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy, lazy2)
import LocalStorage exposing (saveProgText)
import Manual
import Program exposing (Inst(..), Proc, Program, ProgramError, parseProgram)
import ProgramList



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { progText : String, valid : Maybe Program, current : Maybe Program, errors : List ProgramError, plModel : ProgramList.Model }


{-| Initialize app with the program saved in local storage
-}
init : Maybe String -> ( Model, Cmd Msg )
init flags =
    update (ProgramTextUpdated (Maybe.withDefault "" flags))
        { progText = "", valid = Nothing, current = Nothing, errors = [], plModel = ProgramList.init }



-- UPDATE


type Msg
    = ProgramTextUpdated String
    | ProgramListMsg ProgramList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgramListMsg plmsg ->
            ( { model | plModel = ProgramList.update plmsg model.plModel }, Cmd.none )

        ProgramTextUpdated newProgText ->
            let
                -- Try parsing the new program
                ( mprog, errors ) =
                    parseProgram newProgText
            in
            ( { model
                | progText = newProgText

                -- Parsed program, no matter the errors
                , current =
                    case mprog of
                        Just prog ->
                            Just prog

                        Nothing ->
                            model.current
                , valid =
                    if List.isEmpty errors then
                        -- If there are no detected errors we can run the program
                        mprog

                    else
                        -- If there are errors, we keep the last valid program
                        model.valid
                , errors = errors
              }
            , saveProgText newProgText
            )



-- MAIN VIEW


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ div [ class "column" ]
            [ textarea
                [ id "codeEditor"
                , placeholder "Program text"
                , value model.progText
                , onInput ProgramTextUpdated
                , autofocus True
                , cols 80
                , rows 10
                , spellcheck False
                ]
                []
            , ErrorList.view model.errors
            , Html.map (\plmsg -> ProgramListMsg plmsg) (lazy2 ProgramList.view model.plModel model.current)
            , Manual.view
            ]

        -- TODO(guillaume) il faut indiquer quand les composants ne sont plus à jour
        , div [ class "column" ] [ lazy Canvas.view model.valid ]
        ]
