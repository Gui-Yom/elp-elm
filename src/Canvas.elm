module Canvas exposing (view)

import Dict
import Html exposing (Html)
import Program exposing (Inst(..), Proc, Program)
import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (id, style, width, x1, x2, y1, y2)



-- VIEW


type alias Cursor =
    { x : Float, y : Float, angle : Float, color : String, width : Float }


{-| Correct angle in case it overflows
-}
correctAngle : Float -> Float
correctAngle angle =
    if angle >= 360 then
        angle - 360

    else if angle < 0 then
        angle + 360

    else
        angle


{-| Save cursor x, y and angle between op1 and op2 but not color and width
-}
cursorStack : (Cursor -> ( Cursor, List (Svg msg) )) -> (Cursor -> ( Cursor, List (Svg msg) )) -> Cursor -> ( Cursor, List (Svg msg) )
cursorStack op1 op2 cursor =
    let
        ( newCursor, svg ) =
            op1 cursor
    in
    Tuple.mapSecond (List.append svg) (op2 { newCursor | color = cursor.color, width = cursor.width })


{-| Execute program instructions recursively
-}
drawProc : Program -> Proc -> Cursor -> ( Cursor, List (Svg msg) )
drawProc prog proc cursor =
    case proc of
        [] ->
            ( cursor, [] )

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
                    Tuple.mapSecond
                        (List.append
                            [ line
                                [ x1 (String.fromFloat cursor.x)
                                , y1 (String.fromFloat cursor.y)
                                , x2 (String.fromFloat newCursor.x)
                                , y2 (String.fromFloat newCursor.y)
                                , style ("stroke:" ++ cursor.color ++ ";stroke-width:" ++ String.fromFloat cursor.width)
                                ]
                                []
                            ]
                        )
                        (drawProc prog subProc newCursor)

                Left n ->
                    drawProc prog subProc { cursor | angle = correctAngle (cursor.angle - n) }

                Right n ->
                    drawProc prog subProc { cursor | angle = correctAngle (cursor.angle + n) }

                Color col ->
                    drawProc prog subProc { cursor | color = col }

                Width width ->
                    drawProc prog subProc { cursor | width = width }

                Repeat n toRepeat ->
                    if n <= 1 then
                        cursorStack (drawProc prog toRepeat) (drawProc prog subProc) cursor

                    else
                        cursorStack
                            (drawProc prog toRepeat)
                            (drawProc prog ([ Repeat (n - 1) toRepeat ] ++ subProc))
                            cursor

                Call procName ->
                    cursorStack (drawProc prog (Maybe.withDefault [] (Dict.get procName prog))) (drawProc prog subProc) cursor


view : Maybe Program -> Html msg
view mprog =
    svg
        [ id "canvas" ]
        (case mprog of
            Just prog ->
                Tuple.second (drawProc prog (Maybe.withDefault [] (Dict.get "main" prog)) { x = 300, y = 300, angle = 0, color = "#FF0000", width = 2 })

            Nothing ->
                []
        )
