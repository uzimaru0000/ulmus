module Ulmus.BuildIn exposing (..)

import Ulmus.AST exposing (..)


buildIn : List AST
buildIn =
    [ fold
    ]


fold : AST
fold =
    Define
        (Sybl <| Label "fold")
        (list_
            [ Sybl <| Label "f"
            , Sybl <| Label "init"
            , Sybl <| Label "list"
            ]
        )
        (list_
            [ If
                (list_
                    [ Sybl <| Label "eq"
                    , Sybl <| Label "list"
                    , Sybl NIL
                    ]
                )
                (Sybl <| Label "init")
                (list_
                    [ Let
                        (list_
                            [ list_
                                [ Sybl <| Label "head"
                                , list_
                                    [ Sybl <| Label "car"
                                    , Sybl <| Label "list"
                                    ]
                                ]
                            , list_
                                [ Sybl <| Label "tail"
                                , list_
                                    [ Sybl <| Label "cdr"
                                    , Sybl <| Label "list"
                                    ]
                                ]
                            ]
                        )
                        [ list_
                            [ Sybl <| Label "fold"
                            , Sybl <| Label "f"
                            , list_
                                [ Sybl <| Label "f"
                                , Sybl <| Label "init"
                                , Sybl <| Label "head"
                                ]
                            , Sybl <| Label "tail"
                            ]
                        ]
                    ]
                )
            ]
        )
