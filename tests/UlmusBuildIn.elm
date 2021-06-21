module UlmusBuildIn exposing (..)

import Dict
import Expect
import Test exposing (..)
import Ulmus exposing (..)
import Ulmus.AST exposing (..)
import Utils exposing (..)


emptyCtx : Ctx
emptyCtx =
    Dict.fromList []


testFold : Test
testFold =
    describe "fold function"
        [ test "(fold '+ 0 (1 2 3 4)) -> 10" <|
            \_ ->
                Expect.equal
                    (eval emptyCtx <|
                        list_
                            [ Sybl <| Label "fold"
                            , Quote (Sybl <| Label "+")
                            , Sybl <| Num 0
                            , list_
                                [ Sybl <| Num 1
                                , Sybl <| Num 2
                                , Sybl <| Num 3
                                , Sybl <| Num 4
                                ]
                            ]
                    )
                    (Ok (Sybl <| Num 10))
        ]
