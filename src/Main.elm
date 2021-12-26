module Main exposing (main)

import Browser
import Canvas
import Html exposing (Html, div, li, p, text, textarea, ul)
import Html.Attributes exposing (autofocus, class, cols, id, placeholder, rows, value)
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy, lazy2)
import LocalStorage exposing (saveProgText)
import Program exposing (Inst(..), ParseError, Proc, Program, parseProgram)
import ProgramList



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { progText : String, lastSuccessful : Maybe Program, error : Maybe (List ParseError), plModel : ProgramList.Model }


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    update (ProgramTextUpdated (Maybe.withDefault "" flags))
        { progText = "", lastSuccessful = Nothing, error = Nothing, plModel = ProgramList.init }



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
                result =
                    parseProgram newProgText
            in
            ( { model
                | progText = newProgText
                , lastSuccessful =
                    case result of
                        Ok prog ->
                            Just prog

                        Err _ ->
                            model.lastSuccessful
                , error =
                    case result of
                        Ok _ ->
                            Nothing

                        Err err ->
                            Just err
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
                ]
                []
            , case model.error of
                Just err ->
                    p [ id "errorList" ] [ ul [] (List.map (\e -> li [] [ text ("Error: " ++ Debug.toString e) ]) err) ]

                Nothing ->
                    case model.lastSuccessful of
                        Just prog ->
                            Html.map (\plmsg -> ProgramListMsg plmsg) (lazy2 ProgramList.view model.plModel prog)

                        Nothing ->
                            p [] [ text "Please enter a program" ]
            ]
        , div [ class "column" ] [ lazy Canvas.view model.lastSuccessful ]
        ]
