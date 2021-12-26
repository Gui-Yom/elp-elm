module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, div, li, p, text, textarea, ul)
import Html.Attributes exposing (autofocus, class, cols, id, placeholder, rows, value)
import Html.Events exposing (onInput)
import LocalStorage exposing (saveProgText)
import Parser exposing (DeadEnd)
import Program exposing (Inst(..), Proc, Program, parseProgram)
import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (height, style, viewBox, width, x1, x2, y1, y2)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { progText : String, lastSuccessful : Maybe Program, error : Maybe (List DeadEnd) }


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( { progText = Maybe.withDefault "" flags, lastSuccessful = Nothing, error = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = ProgramTextUpdated String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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



-- VIEW (Show Program)


showProgram : Program -> List (Html Msg)
showProgram prog =
    [ ul []
        (List.map
            (\tuple ->
                li [] [ text (Tuple.first tuple), showProc (Tuple.second tuple) ]
            )
            (Dict.toList prog.procs)
        )
    , showProc prog.main
    ]


showProc : Proc -> Html Msg
showProc proc =
    ul []
        (List.map
            (\inst ->
                case inst of
                    Repeat n subProg ->
                        li [] [ text ("Repeat " ++ String.fromInt n), showProc subProg ]

                    other ->
                        li [] [ text (Debug.toString other) ]
            )
            proc
        )



-- VIEW (Draw Program)


type alias Cursor =
    { x : Float, y : Float, angle : Float }


drawProgram : Program -> Cursor -> List (Svg msg)
drawProgram prog cursor =
    drawProc prog prog.main cursor


drawProc : Program -> Proc -> Cursor -> List (Svg msg)
drawProc prog proc cursor =
    case proc of
        [] ->
            []

        inst :: subProc ->
            case inst of
                Forward length ->
                    let
                        newCursor =
                            { cursor
                                | x = cursor.x + length * cos (degrees cursor.angle)
                                , y = cursor.y + length * sin (degrees cursor.angle)
                            }
                    in
                    [ line
                        [ x1 (String.fromFloat cursor.x)
                        , y1 (String.fromFloat cursor.y)
                        , x2 (String.fromFloat newCursor.x)
                        , y2 (String.fromFloat newCursor.y)
                        , style "stroke:rgb(255,0,0);stroke-width:2"
                        ]
                        []
                    ]
                        ++ drawProc prog subProc newCursor

                Left n ->
                    drawProc prog subProc { cursor | angle = correctAngle (cursor.angle - n) }

                Right n ->
                    drawProc prog subProc { cursor | angle = correctAngle (cursor.angle + n) }

                Repeat n toRepeat ->
                    if n <= 1 then
                        drawProc prog (toRepeat ++ subProc) cursor

                    else
                        drawProc prog (toRepeat ++ [ Repeat (n - 1) toRepeat ] ++ subProc) cursor

                Call procName ->
                    drawProc prog (Maybe.withDefault [] (Dict.get procName prog.procs) ++ subProc) cursor



-- TODO proc call


correctAngle : Float -> Float
correctAngle angle =
    if angle >= 360 then
        angle - 360

    else if angle < 0 then
        angle + 360

    else
        angle



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
                    p [ id "errorMsg" ] [ text ("Error: " ++ Debug.toString err) ]

                Nothing ->
                    case model.lastSuccessful of
                        Just prog ->
                            p [] (showProgram prog)

                        Nothing ->
                            p [ id "errorMsg" ] [ text "Please enter a program" ]
            ]
        , div [ class "column" ]
            [ svg
                [ id "canvas", width "500", height "500", viewBox "0 0 500 500" ]
                (case model.lastSuccessful of
                    Just prog ->
                        drawProgram prog { x = 250, y = 250, angle = 0 }

                    Nothing ->
                        []
                )
            ]
        ]
