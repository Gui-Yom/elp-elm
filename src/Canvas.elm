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


drawInst : Program -> Inst -> Cursor -> ( Cursor, List (Svg msg) )
drawInst prog inst cursor =
    case inst of
        Forward length ->
            let
                newCursor =
                    { cursor
                        | x = cursor.x + length * cos (degrees cursor.angle)
                        , y = cursor.y + length * sin (degrees cursor.angle)
                    }
            in
            ( newCursor
            , [ line
                    [ x1 (String.fromFloat cursor.x)
                    , y1 (String.fromFloat cursor.y)
                    , x2 (String.fromFloat newCursor.x)
                    , y2 (String.fromFloat newCursor.y)
                    , style ("stroke:" ++ cursor.color ++ ";stroke-width:" ++ String.fromFloat cursor.width)
                    ]
                    []
              ]
            )

        Left n ->
            ( { cursor | angle = correctAngle (cursor.angle - n) }, [] )

        Right n ->
            ( { cursor | angle = correctAngle (cursor.angle + n) }, [] )

        Color col ->
            ( { cursor | color = col }, [] )

        Width width ->
            ( { cursor | width = width }, [] )

        Repeat n proc ->
            if n <= 1 then
                drawProc prog proc cursor

            else
                let
                    ( newCursor, svg ) =
                        drawProc prog proc cursor
                in
                Tuple.mapSecond ((++) svg) (drawInst prog (Repeat (n - 1) proc) { newCursor | color = cursor.color, width = cursor.width })

        Call procName ->
            drawInst prog (Scope (Maybe.withDefault [] (Dict.get procName prog))) cursor

        Scope scope ->
            let
                ( newCursor, svg ) =
                    drawProc prog scope cursor
            in
            ( { newCursor | color = cursor.color, width = cursor.width }, svg )


{-| Execute a procedure
-}
drawProc : Program -> Proc -> Cursor -> ( Cursor, List (Svg msg) )
drawProc prog proc cursor =
    List.foldl (\inst ( cur, svg ) -> Tuple.mapSecond ((++) svg) (drawInst prog inst cur)) ( cursor, [] ) proc


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
