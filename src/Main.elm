module Main exposing (main)

import Browser
import Html exposing (Html, div, li, p, text, textarea, ul)
import Html.Attributes exposing (autofocus, cols, placeholder, rows, value)
import Html.Events exposing (onInput)
import Parser exposing (DeadEnd)
import Program exposing (Inst(..), Program, pProgram)
import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (height, style, viewBox, width, x1, x2, y1, y2)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { progText : String, lastSuccessful : Maybe Program, error : Maybe (List DeadEnd) }


init : Model
init =
    { progText = "", lastSuccessful = Nothing, error = Nothing }



-- UPDATE


type Msg
    = ProgramTextUpdated String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextUpdated newProgText ->
            let
                result =
                    Parser.run pProgram newProgText
            in
            { model
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


drawProgram : Program -> List (Svg msg)
drawProgram prog =
    let
        cursor =
            { x = 0, y = 0, angle = 0 }
    in
    List.concatMap
        (\inst ->
            case inst of
                Repeat n subProg ->
                    List.concatMap (\_ -> drawProgram subProg) (List.range 1 n)

                Forward n ->
                    [ line
                        [ x1 (String.fromInt cursor.x)
                        , y1 (String.fromInt cursor.y)
                        , x2 (String.fromInt (cursor.x + n))
                        , y2 (String.fromInt cursor.y)
                        , style "stroke:rgb(255,0,0);stroke-width:2"
                        ]
                        []
                    ]

                _ ->
                    []
        )
        prog


view : Model -> Html Msg
view model =
    div [ style "display: flex;" ]
        [ div [ style "flex: 50%; margin: auto;" ]
            [ textarea [ placeholder "Program text", value model.progText, onInput ProgramTextUpdated, autofocus True, cols 80, rows 30 ] []
            , p []
                [ case model.error of
                    Just err ->
                        text ("Error: " ++ Debug.toString err)

                    Nothing ->
                        case model.lastSuccessful of
                            Just prog ->
                                programList prog

                            Nothing ->
                                text "Please enter a program"
                ]
            ]
        , div [ style "flex: 50%; margin: auto;" ]
            [ svg
                [ width "500", height "500", viewBox "0 0 500 500" ]
                (case model.lastSuccessful of
                    Just prog ->
                        drawProgram prog

                    Nothing ->
                        []
                )
            ]
        ]
