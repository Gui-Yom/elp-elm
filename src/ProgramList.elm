module ProgramList exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, li, span, text, ul)
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


showInst : Model -> Inst -> Html Msg
showInst model inst =
    case inst of
        Repeat n proc ->
            li [] [ text ("Repeat " ++ String.fromInt n), showProc model proc ]

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

        Scope proc ->
            li [] [ showProc model proc ]

        other ->
            li [] [ text (Debug.toString other) ]


{-| Render a procedure instruction list as an html list.
-}
showProc : Model -> Proc -> Html Msg
showProc model proc =
    ul [ class "procList" ]
        (List.map
            (\inst ->
                showInst model inst
            )
            proc
        )


view : Model -> Maybe Program -> Html Msg
view model mprog =
    case mprog of
        Just prog ->
            ul [ id "programList" ]
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
                    (Dict.toList prog)
                )

        Nothing ->
            div [] []
