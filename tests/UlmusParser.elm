module UlmusParser exposing (..)

import Expect
import Parser
import Test exposing (..)
import Ulmus exposing (..)
import Ulmus.AST exposing (..)
import Ulmus.Parser exposing (..)
import Utils exposing (..)


testAtomParser : Test
testAtomParser =
    describe "atom parser"
        [ test "nil" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "nil")
                    (Ok NIL)
        , test "t" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "t")
                    (Ok T)
        , test "1" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "1")
                    (Ok <| Num 1)
        , test "1.4" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "1.4")
                    (Ok <| Num 1.4)
        , test "\"hoge\"" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "\"hoge\"")
                    (Ok <| Str "hoge")
        , test "hoge" <|
            \_ ->
                Expect.equal
                    (Parser.run atom "hoge")
                    (Ok <| Label "hoge")
        ]


testQuote : Test
testQuote =
    describe "test quote"
        [ test "'12" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "'12")
                    (Ok (Quote (Sybl <| Num 12)))
        ]


testPair : Test
testPair =
    describe "test pair"
        [ test "(t . t)" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(t . t)")
                    (Ok (Pair (Sybl T) (Sybl T)))
        , test "(f 12)" <|
            \_ ->
                Expect.equal
                    (Parser.run Ulmus.Parser.list "(f 12)")
                    (Ok <|
                        Pair
                            (Sybl <| Label "f")
                            (Pair
                                (Sybl <| Num 12)
                                (Sybl NIL)
                            )
                    )
        , test "(eq x 15)" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(eq x 15)")
                    (Ok <|
                        Pair
                            (Sybl <| Label "eq")
                            (Pair
                                (Sybl <| Label "x")
                                (Pair
                                    (Sybl <| Num 15)
                                    (Sybl NIL)
                                )
                            )
                    )
        , test "((eq x 15) \"just 15\")" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "((eq x 15) \"just 15\")")
                    (Ok <|
                        Pair
                            (Pair
                                (Sybl <| Label "eq")
                                (Pair
                                    (Sybl <| Label "x")
                                    (Pair
                                        (Sybl <| Num 15)
                                        (Sybl NIL)
                                    )
                                )
                            )
                            (Pair
                                (Sybl <| Str "just 15")
                                (Sybl NIL)
                            )
                    )
        ]


testLambda : Test
testLambda =
    describe "test lambda"
        [ test "lambda (x) x" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "lambda (x) x")
                    (Ok <|
                        Lambda
                            (Pair (Sybl <| Label "x") (Sybl NIL))
                            (Sybl <| Label "x")
                    )
        , test "(lambda (x) (+ x 1))" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(lambda (x) (+ x 1))")
                    (Ok <|
                        list_
                            [ Lambda
                                (list_
                                    [ Sybl <| Label "x"
                                    ]
                                )
                                (list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 1
                                    ]
                                )
                            ]
                    )
        ]


testLet : Test
testLet =
    describe "test let"
        [ test "let ((x 1)) (+ x 1)" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "let ((x 1)) (+ x 1)")
                    (Ok <|
                        Let
                            (list_
                                [ list_
                                    [ Sybl <| Label "x"
                                    , Sybl <| Num 1
                                    ]
                                ]
                            )
                            [ list_
                                [ Sybl <| Label "+"
                                , Sybl <| Label "x"
                                , Sybl <| Num 1
                                ]
                            ]
                    )
        , test "(let ((x 1)) (+ x 1))" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(let ((x 1)) (+ x 1))")
                    (Ok <|
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "x"
                                        , Sybl <| Num 1
                                        ]
                                    ]
                                )
                                [ list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 1
                                    ]
                                ]
                            ]
                    )
        , test "(let ((x 1)) (+ x 1) (+ x 2))" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(let ((x 1)) (+ x 1) (+ x 2))")
                    (Ok <|
                        list_
                            [ Let
                                (list_
                                    [ list_
                                        [ Sybl <| Label "x"
                                        , Sybl <| Num 1
                                        ]
                                    ]
                                )
                                [ list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 1
                                    ]
                                , list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 2
                                    ]
                                ]
                            ]
                    )
        ]


testIf : Test
testIf =
    describe "test if" <|
        [ test "(if (< 1 2) nil t)" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(if (< 1 2) nil t)")
                    (Ok <|
                        list_
                            [ If
                                (list_
                                    [ Sybl <| Label "<"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 2
                                    ]
                                )
                                (Sybl NIL)
                                (Sybl T)
                            ]
                    )
        , test "(if (< 1 2) (+ 1 2) (- 1 2))" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(if (< 1 2) (+ 1 2) (- 1 2))")
                    (Ok <|
                        list_
                            [ If
                                (list_
                                    [ Sybl <| Label "<"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 2
                                    ]
                                )
                                (list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 2
                                    ]
                                )
                                (list_
                                    [ Sybl <| Label "-"
                                    , Sybl <| Num 1
                                    , Sybl <| Num 2
                                    ]
                                )
                            ]
                    )
        ]


testDefine : Test
testDefine =
    describe "test Define"
        [ test "define id (x) x" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "define id (x) x")
                    (Ok <|
                        Define
                            (Sybl <| Label "id")
                            (Pair (Sybl <| Label "x") (Sybl NIL))
                            (Sybl <| Label "x")
                    )
        , test "(define succ (x) (+ x 1))" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "(define succ (x) (+ x 1))")
                    (Ok <|
                        list_
                            [ Define
                                (Sybl <| Label "succ")
                                (list_
                                    [ Sybl <| Label "x"
                                    ]
                                )
                                (list_
                                    [ Sybl <| Label "+"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 1
                                    ]
                                )
                            ]
                    )
        , test "(define 1 (x) (+ x 1) -> err" <|
            \_ ->
                Expect.err
                    (Parser.run sExp "(define 1 (x) (+ x 1))")
        ]


testCond : Test
testCond =
    describe "test Cond"
        [ test "cond ((eq x 15) \"just 15\") ((eq x 5) \"just 5\") ((eq x 3) \"just 3\") (else \"other\")" <|
            \_ ->
                Expect.equal
                    (Parser.run sExp "cond ((eq x 15) \"just 15\") ((eq x 5) \"just 5\") ((eq x 3) \"just 3\") (else \"other\")")
                    (Ok <|
                        Cond
                            [ list_
                                [ list_
                                    [ Sybl <| Label "eq"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 15
                                    ]
                                , Sybl <| Str "just 15"
                                ]
                            , list_
                                [ list_
                                    [ Sybl <| Label "eq"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 5
                                    ]
                                , Sybl <| Str "just 5"
                                ]
                            , list_
                                [ list_
                                    [ Sybl <| Label "eq"
                                    , Sybl <| Label "x"
                                    , Sybl <| Num 3
                                    ]
                                , Sybl <| Str "just 3"
                                ]
                            ]
                            (Sybl <| Str "other")
                    )
        ]


program : String
program =
    """
(define fact (x)
    (if (<= x 1)
        x
        (* x (fact (- x 1)))
    )
)

;; 10! = 3628800
(fact 10)
"""


testFactProgram : Test
testFactProgram =
    test "fact program" <|
        \_ ->
            Expect.equal
                (Parser.run parser program)
                (Ok
                    [ list_
                        [ Define
                            (Sybl <| Label "fact")
                            (list_ [ Sybl <| Label "x" ])
                            (list_
                                [ If
                                    (list_
                                        [ Sybl <| Label "<="
                                        , Sybl <| Label "x"
                                        , Sybl <| Num 1
                                        ]
                                    )
                                    (Sybl <| Label "x")
                                    (list_
                                        [ Sybl <| Label "*"
                                        , Sybl <| Label "x"
                                        , list_
                                            [ Sybl <| Label "fact"
                                            , list_
                                                [ Sybl <| Label "-"
                                                , Sybl <| Label "x"
                                                , Sybl <| Num 1
                                                ]
                                            ]
                                        ]
                                    )
                                ]
                            )
                        ]
                    , list_
                        [ Sybl <| Label "fact"
                        , Sybl <| Num 10
                        ]
                    ]
                )
