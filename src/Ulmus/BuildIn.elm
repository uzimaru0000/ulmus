module Ulmus.BuildIn exposing (..)

import Ulmus.AST exposing (..)


buildIn : List (String, AST)
buildIn =
    [ ("not", not)
    , ("null", not)
    ]


not : AST
not =
    Lambda
        (list_
            [ Sybl <| Label "x" ]
        )
        (list_
            [ If
                (Sybl <| Label "x")
                (Sybl NIL)
                (Sybl T)
            ]
        )

