module Main exposing (main)

import Browser
import Html exposing (Html, div, li, p, text, textarea, ul)
import Html.Attributes exposing (autofocus, class, cols, id, placeholder, rows, value)
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
    ul [ id "programList" ]
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


type alias Cursor =
    { x : Float, y : Float, angle : Float }


drawProgram : Program -> Cursor -> List (Svg msg)
drawProgram prog cursor =
    case prog of
        [] ->
            []

        inst :: subprog ->
            case inst of
                Repeat n toRepeat ->
                    if n <= 1 then
                        drawProgram (toRepeat ++ subprog) cursor

                    else
                        drawProgram (toRepeat ++ [ Repeat (n - 1) toRepeat ] ++ subprog) cursor

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
                        ++ drawProgram subprog newCursor

                Left n ->
                    drawProgram subprog { cursor | angle = correctAngle (cursor.angle - n) }

                Right n ->
                    drawProgram subprog { cursor | angle = correctAngle (cursor.angle + n) }


correctAngle : Float -> Float
correctAngle angle =
    if angle >= 360 then
        angle - 360

    else if angle < 0 then
        angle + 360

    else
        angle


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
                            programList prog

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
