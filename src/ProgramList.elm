module ProgramList exposing (..)

import Dict
import Html exposing (Html, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onMouseOut, onMouseOver)
import Program exposing (Inst(..), Proc, Program)



-- MODEL


type alias Model =
    { procRefHovered : Maybe String, procDefHovered : Maybe String }


init : Model
init =
    { procRefHovered = Nothing, procDefHovered = Nothing }



-- UPDATE


type Msg
    = ProcRefHover String
    | ProcDefHover String
    | MouseOut


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProcRefHover hovering ->
            { model | procRefHovered = Just hovering }

        ProcDefHover hovering ->
            { model | procDefHovered = Just hovering }

        MouseOut ->
            { model | procRefHovered = Nothing, procDefHovered = Nothing }



-- VIEW


{-| Render a procedure instruction list as an html list.
-}
showProc : Model -> Proc -> Html Msg
showProc model proc =
    ul [ class "procList" ]
        (List.map
            (\inst ->
                case inst of
                    Repeat n subProg ->
                        li [] [ text ("Repeat " ++ String.fromInt n), showProc model subProg ]

                    Call procName ->
                        li []
                            [ text "Call "
                            , span
                                [ classList
                                    [ ( "procRef", True )
                                    , ( "procHover", Maybe.withDefault False (Maybe.map (\def -> def == procName) model.procDefHovered) )
                                    ]

                                -- Events
                                , onMouseOver (ProcRefHover procName)
                                , onMouseOut MouseOut
                                ]
                                [ text procName ]
                            ]

                    other ->
                        li [] [ text (Debug.toString other) ]
            )
            proc
        )


view : Model -> Program -> Html Msg
view model prog =
    p [ id "programList" ]
        [ ul []
            (List.map
                (\tuple ->
                    let
                        ( name, proc ) =
                            tuple
                    in
                    li []
                        [ span
                            [ classList
                                [ ( "procDef", True )
                                , ( "procHover", Maybe.withDefault False (Maybe.map (\def -> def == name) model.procRefHovered) )
                                ]

                            -- Events
                            , onMouseOver (ProcDefHover name)
                            , onMouseOut MouseOut
                            ]
                            [ text name ]
                        , showProc model proc
                        ]
                )
                (Dict.toList prog.procs)
            )
        , showProc model prog.main
        ]
