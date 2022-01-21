module Manual exposing (view)

import Html exposing (details, li, p, pre, summary, text, ul)
import Html.Attributes exposing (class, id)


br =
    Html.br [] []


h1 txt =
    Html.h1 [] [ text txt ]


h2 txt =
    Html.h2 [] [ text txt ]


h3 txt =
    Html.h3 [] [ text txt ]


code text =
    pre [] [ Html.code [ class "codeDisplay" ] text ]


view =
    details [ id "manual" ]
        [ summary [] [ text "Manuel" ]
        , h2 "Programme"
        , p []
            [ text "instruction: Forward float | Left float | Right float | Repeat int proc | Call name | Color color | Width float"
            , br
            , text "proc: [instruction,...]"
            , br
            , text "procDef: name proc"
            ]
        , h3 "Exemple"
        , code [ text "circle [Repeat 15 [Forward 10, Left 24]]", br, text "[Call circle]" ]
        , h2 "Instructions"
        , h3 "Color <color>"
        , p []
            [ text "<color> est sous une des formes suivantes :"
            , ul []
                [ li [] [ text "Code couleur hexadécimal, ex. #24FEB3" ]
                , li [] [ text "Fonctions couleur CSS, ex. rgb(45, 78, 255) or hsl(46, 89, 12)" ]
                , li [] [ text "Nom de couleur CSS, ex. beige" ]
                ]
            ]
        ]
