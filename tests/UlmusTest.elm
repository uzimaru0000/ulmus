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
        , test "(quote (+ 1 2)) -> (+ 1 2)" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "quote"
                            , list_
                                [ Sybl <| Label "+"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                    )
                    (Ok <|
                        list_
                            [ Sybl <| Label "+"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
        , test "(lambda (x) (x))" <|
            \_ ->
                Expect.equal
                    (eval <|
                        Lambda
                            (list_ [ Sybl <| Label "x" ])
                            (list_ [ Sybl <| Label "x" ])
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
        [ test "(let ((x 1)) x) -> 1" <|
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
        , test "変数２個" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "x"
                                        , Sybl <| Num 3
                                        ]
                                    , list_
                                        [ Sybl <| Label "y"
                                        , Sybl <| Num 7
                                        ]
                                    ]
                                )
                                [ list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Label "y"
                                    ]
                                ]
                            ]
                    )
                    (Ok (Sybl <| Num 10))
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


testAdd : Test
testAdd =
    describe "+"
        [ test "(+ 1 2) -> 3" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "+"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 3))
        , test "(+ 1 2 3 4 5) -> 15" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "+"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            , Sybl <| Num 4
                            , Sybl <| Num 5
                            ]
                    )
                    (Ok (Sybl <| Num 15))
        , test "(+ 1) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "+"
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        , test "(+) -> 0" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "+"
                            ]
                    )
                    (Ok (Sybl <| Num 0))
        ]

testMul : Test
testMul =
    describe "*"
        [ test "(* 1 2) -> 2" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "*"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 2))
        , test "(* 1 2 3 4 5) -> 120" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "*"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            , Sybl <| Num 4
                            , Sybl <| Num 5
                            ]
                    )
                    (Ok (Sybl <| Num 120))
        , test "(* 1) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "*"
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        , test "(*) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "*"
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        ]

testSub : Test
testSub =
    describe "-"
        [ test "(- 1 2) -> -1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "-"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num -1))
         , test "(- 1 2 3 4 5) -> -13" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "-"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            , Sybl <| Num 4
                            , Sybl <| Num 5
                            ]
                    )
                    (Ok (Sybl <| Num -13))
        , test "(- 1) -> -1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "-"
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok (Sybl <| Num -1))
        , test "(-) -> err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label "-"
                            ]
                    )
        ]

testDiv : Test
testDiv =
    describe "/"
        [ test "(/ 1 2) -> 0.5" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "/"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 0.5))
         , test "(/ 1 2 3 4 5) -> 0.008333333" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "/"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            , Sybl <| Num 4
                            , Sybl <| Num 5
                            ]
                    )
                    (Ok (Sybl <| Num 0.008333333333333333))
        , test "(/ 2) -> 0.5" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "/"
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 0.5))
        , test "(/) -> err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label "/"
                            ]
                    )
        ]


testMod : Test
testMod =
    describe "mod" <|
        [ test "(mod 1 2) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "mod"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        , test "(mod 15 3) -> 0" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "mod"
                            , Sybl <| Num 15
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok (Sybl <| Num 0))
        , test "(mod 11 5) -> 1" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "mod"
                            , Sybl <| Num 11
                            , Sybl <| Num 5
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        ]


testComp : Test
testComp =
    describe "comparison operator"
        [ test "(< 1 2) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(< 1 2 3) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(< 1 1) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<"
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(<) -> Err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label "<" ]
                    )
        , test "(> 1 2) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(> 1 2 3) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(> 1 1) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">"
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(>) -> Err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label ">" ]
                    )
        , test "(<= 1 2) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<="
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(<= 1 2 3) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<="
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(<= 1 1) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "<="
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(<=) -> Err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label "<=" ]
                    )
        , test "(>= 1 2) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">="
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(>= 1 2 3) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">="
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(>= 1 1) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label ">="
                            , Sybl <| Num 1
                            , Sybl <| Num 1
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(>=) -> Err" <|
            \_ ->
                Expect.err
                    (eval <|
                        list_
                            [ Sybl <| Label ">=" ]
                    )
        ]

testRecursive : Test
testRecursive =
    let
        fact n = list_
                [ Let
                    (list_
                        [ list_
                            [ Sybl <| Label "fact"
                            , Lambda
                                (list_ [ Sybl <| Label "n" ])
                                (list_
                                    [ If
                                        (list_
                                            [ Sybl <| Label "<"
                                            , Sybl <| Label "n"
                                            , Sybl <| Num 1
                                            ]
                                        )
                                        (Sybl <| Num 1)
                                        (list_
                                            [ Sybl <| Label "*"
                                            , Sybl <| Label "n"
                                            , list_
                                                [ Sybl <| Label "fact"
                                                , list_
                                                    [ Sybl <| Label "-"
                                                    , Sybl <| Label "n"
                                                    , Sybl <| Num 1
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                                )
                            ]
                        ]
                    )
                    [ list_
                        [ Sybl <| Label "fact"
                        , Sybl <| Num n
                        ]
                    ]
                ]
    in
    describe "recursive func"
        [ test "fact 1 = 1" <|
            \_ ->
                Expect.equal
                    (eval <| fact 1)
                    (Ok (Sybl <| Num 1))
        , test "fact 2 = 2" <|
            \_ ->
                Expect.equal
                    (eval <| fact 2)
                    (Ok (Sybl <| Num 2))
        , test "fact 3 = 6" <|
            \_ ->
                Expect.equal
                    (eval <| fact 3)
                    (Ok (Sybl <| Num 6))
        ]
            
testLogicFunc : Test
testLogicFunc =
    describe "logic functions"
        [ test "(or 1 2) -> 2" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "or"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl <| Num 1))
        , test "(or) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "or" ]
                    )
                    (Ok (Sybl NIL))
        , test "(or NIL (+ 1 2)) -> 3" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "or"
                            , Sybl NIL
                            , list_
                                [ Sybl <| Label "+"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                    )
                    (Ok (Sybl <| Num 3))
        , test "(or T (hoge 1 2)) -> T" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Sybl <| Label "or"
                            , Sybl T
                            , list_
                                [ Sybl <| Label "hoge"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                    )
                    (Ok <| Sybl T)
        , test "(and 1 NIL 2) -> NIL" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "and"
                            , Sybl <| Num 1
                            , Sybl NIL
                            , Sybl <| Num 2
                            ]
                    )
                    (Ok (Sybl NIL))
        , test "(and 1 2 3) -> 3" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "and"
                            , Sybl <| Num 1
                            , Sybl <| Num 2
                            , Sybl <| Num 3
                            ]
                    )
                    (Ok (Sybl <| Num 3))
        , test "(and T (+ 1 2)) -> 3" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "and"
                            , Sybl T
                            , list_
                                [ Sybl <| Label "+"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                    )
                    (Ok (Sybl <| Num 3))
        , test "(and NIL (hoge 1 2)) -> 3" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "and"
                            , Sybl NIL
                            , list_
                                [ Sybl <| Label "hoge"
                                , Sybl <| Num 1
                                , Sybl <| Num 2
                                ]
                            ]
                    )
                    (Ok <| Sybl NIL)
        , test "(and) -> T" <|
            \_ ->
                Expect.equal
                    (eval  <|
                        list_
                            [ Sybl <| Label "and" ]
                    )
                    (Ok <| Sybl T)
        ]


testDefine : Test
testDefine =
    describe "define test"
        [ test "define id func" <|
            \_ ->
                Expect.equal
                    (evalAll
                        [ Define
                            (Sybl <| Label "id")
                            (list_
                                [ Sybl <| Label "x"
                                ]
                            )
                            (Sybl <| Label "x")
                        , list_
                            [ Sybl <| Label "id"
                            , Sybl <| Num 1
                            ]
                        ]
                        |> Result.map (Tuple.first)
                    )
                    (Ok (Sybl <| Num 1))
        , test "define fact func" <|
            \_ ->
                Expect.equal
                    (evalAll
                        [ Define
                            (Sybl <| Label "fact")
                            (list_
                                [ Sybl <| Label "n" ]
                            )
                            (list_
                                [ If
                                    (list_
                                        [ Sybl <| Label "<"
                                        , Sybl <| Label "n"
                                        , Sybl <| Num 1
                                        ]
                                    )
                                    (Sybl <| Num 1)
                                    (list_
                                        [ Sybl <| Label "*"
                                        , Sybl <| Label "n"
                                        , list_
                                            [ Sybl <| Label "fact"
                                            , list_
                                                [ Sybl <| Label "-"
                                                , Sybl <| Label "n"
                                                , Sybl <| Num 1
                                                ]
                                            ]
                                        ]
                                    )
                                ]
                            )
                        , list_
                            [ Sybl <| Label "fact"
                            , Sybl <| Num 5
                            ]
                        ]
                        |> Result.map (Tuple.first)
                    )
                    (Ok (Sybl <| Num 120))
        ]


testCond : Test
testCond =
    describe "cond"
        [ test "basic" <|
            \_ ->
                Expect.equal
                    (eval <|
                        list_
                            [ Cond
                                [ list_
                                    [ list_
                                        [ Sybl <| Label "eq"
                                        , list_
                                            [ Sybl <| Label "mod" 
                                            , Sybl <| Num 4
                                            , Sybl <| Num 15
                                            ]
                                        , Sybl <| Num 0
                                        ]
                                    , Sybl <| Str "fizzbuzz"
                                    ]
                                , list_
                                    [ list_
                                        [ Sybl <| Label "eq"
                                        , list_
                                            [ Sybl <| Label "mod" 
                                            , Sybl <| Num 4
                                            , Sybl <| Num 3
                                            ]
                                        , Sybl <| Num 0
                                        ]
                                    , Sybl <| Str "fizz"
                                    ]
                                , list_
                                    [ list_
                                        [ Sybl <| Label "eq"
                                        , list_
                                            [ Sybl <| Label "mod" 
                                            , Sybl <| Num 4
                                            , Sybl <| Num 5
                                            ]
                                        , Sybl <| Num 0
                                        ]
                                    , Sybl <| Str "buzz"
                                    ]
                                ]
                                (Sybl <| Num 4)
                            ]
                    )
                    (Ok (Sybl <| Num 4))
        ]

