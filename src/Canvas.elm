module Canvas exposing (..)

import Dict
import Html exposing (Html)
import Program exposing (Inst(..), Proc, Program)
import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (height, id, style, viewBox, width, x1, x2, y1, y2)



-- VIEW


type alias Cursor =
    { x : Float, y : Float, angle : Float, color : String }


correctAngle : Float -> Float
correctAngle angle =
    if angle >= 360 then
        angle - 360

    else if angle < 0 then
        angle + 360

    else
        angle


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
                        , style ("stroke:" ++ cursor.color ++ ";stroke-width:2")
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
                    drawProc prog (Maybe.withDefault [] (Dict.get procName prog) ++ subProc) cursor

                Color col ->
                    drawProc prog subProc { cursor | color = col }


view : Maybe Program -> Html msg
view mprog =
    svg
        [ id "canvas", width "500", height "500", viewBox "0 0 500 500" ]
        (case mprog of
            Just prog ->
                drawProc prog (Maybe.withDefault [] (Dict.get "main" prog)) { x = 250, y = 250, angle = 0, color = "#FF0000" }

            Nothing ->
                []
        )
