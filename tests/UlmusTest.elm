module UlmusTest exposing (..)

import Expect
import Test exposing (..)
import Ulmus exposing (..)
import Ulmus.AST exposing (..)


testShow : Test
testShow =
    describe "show"
        [ test "nil" <|
            \_ ->
                Expect.equal
                    (show <| Sybl NIL)
                    "()"
        , test "t" <|
            \_ ->
                Expect.equal
                    (show <| Sybl T)
                    "t"
        , test "num" <|
            \_ ->
                Expect.equal
                    (show <| Sybl (Num 1))
                    "1"
        , test "string" <|
            \_ ->
                Expect.equal
                    (show <| Sybl (Str "a"))
                    "\"a\""
        , test "Pair" <|
            \_ ->
                Expect.equal
                    (show <| Pair (Sybl <| Str "a") (Sybl NIL))
                    "(\"a\" ())"
        ]
    

testListHelper : Test
testListHelper =
    describe "list helper"
        [ test "(cons 1 2)" <|
            \_ ->
                Expect.equal
                    (list_
                        [ Sybl <| Str "cons"
                        , Sybl <| Num 1
                        , Sybl <| Num 2
                        ]
                    )
                    (Pair
                        (Sybl <| Str "cons")
                        (Pair
                            (Sybl <| Num 1)
                            (Pair
                                (Sybl <| Num 2)
                                (Sybl NIL)
                            )
                        )
                    )
        , test "(cdr (cons 1 2))" <|
            \_ ->
                Expect.equal
                    (list_
                        [ Sybl <| Label "cdr"
                        , list_
                            [ Sybl <| Label "cons"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                        ]
                    )
                    (Pair
                        (Sybl <| Label "cdr")
                        (Pair
                            (Pair
                                (Sybl <| Label "cons")
                                (Pair
                                    (Sybl <| Num 1)
                                    (Pair
                                        (Sybl <| Num 2)
                                        (Sybl NIL)
                                    )
                                )
                            )
                            (Sybl NIL)
                        )
                    )
        ]
    

testListLen : Test
testListLen =
    describe "list len"
        [ test "len (1 2 3) -> 3" <|
            \_ ->
                Expect.equal
                    (list_
                        [ Sybl <| Num 1
                        , Sybl <| Num 2
                        , Sybl <| Num 3
                        ]
                        |> len_
                    )
                    3
        , test "len () -> 0" <|
            \_ ->
                Expect.equal
                    (list_
                        []
                        |> len_
                    )
                    0
        ]


testBuildIn : Test
testBuildIn =
    describe "build in"
        [ test "cons" <|
            \_ ->
                Expect.equal
                    (cons <|
                        list_
                            [ Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok <|
                        Pair ( Sybl <| Num 1 ) ( Sybl <| Num 2 )
                    )
        , test "car" <|
            \_ ->
                Expect.equal
                    (car <| list_ [ Pair ( Sybl <| Num 1 ) ( Sybl <| Num 2 ) ])
                    (Ok (Sybl <| Num 1))
        ]

    
testEval : Test
testEval =
    describe "eval test"
        [ test "()" <|
            \_ ->
                Expect.equal
                    (eval (Sybl NIL))
                    (Ok <| Sybl NIL)
        , test "t" <|
            \_ ->
                Expect.equal
                    (eval (Sybl T))
                    (Ok <| Sybl T)
        , test "1" <|
            \_ ->
                Expect.equal
                    (eval (Sybl <| Num 1))
                    (Ok <| Sybl <| Num 1)
        , test "str" <|
            \_ ->
                Expect.equal
                    (eval (Sybl <| Str "a"))
                    (Ok <| Sybl <| Str "a")
        , test "'(T . NIL)" <|
            \_ ->
                Expect.equal
                    (eval (Quote <| Pair (Sybl T) (Sybl NIL)))
                    (Ok <| Pair (Sybl T) (Sybl NIL))
        , test "(lambda (x) (x))" <|
            \_ ->
                Expect.equal
                    (eval
                        (Lambda
                            (list_ [ Sybl <| Label "x" ])
                            (list_ [ Sybl <| Label "x" ])
                        )
                    )
                    (Ok <|
                        Lambda
                            (list_ [ Sybl <| Label "x" ])
                            (list_ [ Sybl <| Label "x" ])
                    )
        , test "(cons 1 2) -> (1 . 2)" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "cons"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Pair (Sybl <| Num 1) (Sybl <| Num 2)))
        , test "(car (cons 1 2)) -> 1" <|
            \_ ->
                Expect.equal
                    (eval 
                        (list_
                            [ Sybl <| Label "car"
                            , list_
                                [ Sybl <| Label "cons"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                        )
                    )
                    (Ok (Sybl <| Num 1))
        , test "(cdr (cons 1 2)) -> 2" <|
            \_ ->
                Expect.equal
                    (eval 
                        (Pair
                            (Sybl <| Label "cdr")
                            (Pair
                                (Pair
                                    (Sybl <| Label "cons")
                                    (Pair
                                        (Sybl <| Num 1)
                                        (Pair
                                            (Sybl <| Num 2)
                                            (Sybl NIL)
                                        )
                                    )
                                )
                                (Sybl NIL)
                            )
                        )
                    )
                    (Ok (Sybl <| Num 2))
        , test "(car '(1 2 3)) -> 1" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Sybl <| Label "car"
                            , list_
                                [ Sybl <| Num 1 
                                , Sybl <| Num 2
                                , Sybl <| Num 3
                                ]
                            ]
                        )
                    )
                    (Ok (Sybl <| Num 1))
        , test "(cdr '(1 2 3)) -> (2 3)" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Sybl <| Label "cdr"
                            , list_
                                [ Sybl <| Num 1 
                                , Sybl <| Num 2
                                , Sybl <| Num 3
                                ]
                            ]
                        )
                    )
                    (Ok
                        (list_
                            [ Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                        )
                    )
        , test "(cons 1 2 3) -> Err" <|
            \_ ->
                Expect.err
                    (eval
                        (list_
                            [ Sybl <| Label "cons"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                        )
                    )
        , test "(eq 1 1) -> T" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Sybl <| Label "eq"
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                        )
                    )
                    (Ok <| Sybl T)
        , test "(eq 1 2) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Sybl <| Label "eq"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                        )
                    )
                    (Ok <| Sybl NIL)
        , test "(eq 1 2 3) -> Err" <|
            \_ ->
                Expect.err
                    (eval
                        (list_
                            [ Sybl <| Label "eq"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                        )
                    )
        ]


complex : Test
complex =
    describe "complex case"
        [ test "(car (cdr (list 10 20 30))) -> 20" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Sybl <| Label "car"
                            , list_
                                [ Sybl <| Label "cdr"
                                , list_
                                    [ Sybl <| Num 10
                                    , Sybl <| Num 20
                                    , Sybl <| Num 30
                                    ]
                                ]
                            ]
                        )
                    )
                    (Ok (Sybl <| Num 20))
        , test "((lambda (x) (x)) 10) -> 10" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Lambda
                                (list_ [ Sybl <| Label "x" ])
                                (Sybl <| Label "x")
                            , Sybl <| Num 10 
                            ]
                        )
                    )
                    (Ok (Sybl <| Num 10)
                    )
        , test "((lambda (x) (car (cdr x))) '(abc def ghi)) -> \"def\"" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Lambda
                                (list_ [ Sybl <| Label "x" ])
                                (list_
                                    [ Sybl <| Label "car"
                                    , list_
                                        [ Sybl <| Label "cdr"
                                        , Sybl <| Label "x"
                                        ]
                                    ]
                                )
                            , list_
                                [ Sybl <| Str "abc"
                                , Sybl <| Str "def"
                                , Sybl <| Str "ghi"
                                ]
                            ]
                        )
                    )
                    (Ok (Sybl <| Str "def"))
        , test "((lambda (f x y) (f x (f y '()))) 'cons '10 '20) -> (10 20)" <|
            \_ ->
                Expect.equal
                    (eval
                        (list_
                            [ Lambda
                                (list_
                                    [ Sybl <| Label "f"
                                    , Sybl <| Label "x"
                                    , Sybl <| Label "y"
                                    ]
                                )
                                (list_
                                    [ Sybl <| Label "f"
                                    , Sybl <| Label "x"
                                    , list_
                                        [ Sybl <| Label "f"
                                        , Sybl <| Label "y"
                                        , Sybl NIL
                                        ]
                                    ]
                                )
                            , Quote <| Sybl <| Label "cons"
                            , Quote <| Sybl <| Num 10
                            , Quote <| Sybl <| Num 20
                            ]
                        )
                    )
                    (Ok
                        (list_
                            [ Sybl <| Num 10
                            , Sybl <| Num 20
                            ]
                        )
                    )
        ]

testLet : Test
testLet =
    describe "test let" <|
        [ test "(cons 1 1) -> (1 . 1)" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "cons"
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok (Pair (Sybl <| Num 1) (Sybl <| Num 1))) 
        , test "(let ((x 1)) x) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "x"
                                        , Sybl <| Num 1
                                        ]
                                    ]
                                )
                                [Sybl <| Label "x"]
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        , test "(let ((x 2)) (let ((y 3)) (cons x y))) -> (2 . 3)" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "x"
                                        , Sybl <| Num 1
                                        ]
                                    ]
                                )
                                [list_
                                    [ Let
                                        (list_
                                            [ list_
                                                [ Sybl <| Label "y"
                                                , Sybl <| Num 3
                                                ]
                                            ]
                                        )
                                        [list_
                                            [ Sybl <| Label "cons"
                                            , Sybl <| Label "x"
                                            , Sybl <| Label "y"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                    )
                    (Ok (Pair (Sybl <| Num 1) (Sybl <| Num 3)))
        , test "複数の式" <|
            \_ ->
                let
                    e =
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "a"
                                        , Sybl <| Num 20
                                        ]
                                    ]
                                )
                                [ Sybl <| Label "a"
                                , list_
                                    [ Sybl <| Label "cons"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 10
                                    ]
                                ]
                            ]
                in
                Expect.equal
                    (eval e)
                    (Ok <| Pair (Sybl <| Num 1) (Sybl <| Num 10)) 
        , test "レキシカルスコープ" <|
            \_ ->
                let
                    e =
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "a"
                                        , Sybl <| Num 10
                                        ]
                                    ]
                                )
                                [ list_
                                    [ Let
                                        (list_
                                            [ list_
                                                [ Sybl <| Label "a"
                                                , Sybl <| Num 20
                                                ]
                                            ]
                                        )
                                        [ Sybl <| Label "a" ]
                                    ]
                                , list_
                                    [ Sybl <| Label "cons"
                                    , Sybl <| Label "a"
                                    , Sybl <| Num 10
                                    ]
                                ]
                            ]
                in
                Expect.equal
                    (eval e)
                    (Ok <| Pair (Sybl <| Num 10) (Sybl <| Num 10))
        ]


testIf : Test
testIf =
    describe "if"
        [ test "(if (eq 1 2) (1) (2)) -> " <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ If
                                (list_
                                    [ Sybl <| Label "eq"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 2
                                    ]
                                )
                                (Sybl <| Num 1)
                                (Sybl <| Num 2)
                            ]
                    )
                    (Ok (Sybl <| Num 2))
        ]
