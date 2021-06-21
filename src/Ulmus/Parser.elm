module Ulmus.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser, oneOf)
import Ulmus.AST exposing (..)


parser : Parser (List AST)
parser =
    Parser.succeed identity
        |. blank
        |= Parser.loop
            []
            (\st ->
                oneOf
                    [ Parser.succeed (\stmt -> Parser.Loop <| stmt :: st)
                        |= sExp
                        |. blank
                    , Parser.end
                        |> Parser.map (\_ -> Parser.Done <| List.reverse st)
                    ]
            )


sExp : Parser AST
sExp =
    Parser.oneOf
        [ quote
        , lambda
        , let_
        , if_
        , define
        , cond
        , atom |> Parser.map Sybl
        , pair
        ]


atom : Parser Atom
atom =
    Parser.oneOf
        [ Parser.succeed NIL
            |. Parser.keyword "nil"
        , Parser.succeed T
            |. Parser.keyword "t"
        , Parser.backtrackable <| numAtom
        , strAtom
        , labelAtom
        ]


numAtom : Parser Atom
numAtom =
    Parser.succeed Num
        |= Parser.float


strAtom : Parser Atom
strAtom =
    (Parser.succeed identity
        |. Parser.token "\""
        |= Parser.loop [] stringHelp
    )
        |> Parser.andThen
            (\res ->
                case res of
                    Ok str ->
                        Parser.succeed str

                    Err msg ->
                        Parser.problem msg
            )
        |> Parser.map Str


stringHelp : List String -> Parser.Parser (Parser.Step (List String) (Result String String))
stringHelp revChunks =
    Parser.oneOf
        [ Parser.end
            |> Parser.map
                (\_ -> Parser.Done (Err "string ended prematurely"))
        , Parser.token "\""
            |> Parser.map
                (\_ -> Parser.Done (Ok (String.join "" (List.reverse revChunks))))
        , Parser.chompWhile (\c -> c /= '\\' && c /= '"')
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


labelAtom : Parser Atom
labelAtom =
    Parser.succeed Label
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (\c -> List.all ((/=) c) [ '(', ')', '\'' ])
                    |. Parser.chompWhile (\c -> Char.isAlpha c || c == '_')
           )


quote : Parser AST
quote =
    Parser.succeed Quote
        |. Parser.symbol "'"
        |= Parser.lazy (\_ -> sExp)


pair : Parser AST
pair =
    Parser.oneOf
        [ Parser.backtrackable <| dotPair
        , list
        ]


list : Parser AST
list =
    Parser.succeed identity
        |. Parser.symbol "("
        |. blank
        |= Parser.loop
            []
            (\st ->
                Parser.oneOf
                    [ Parser.succeed (\stmt -> Parser.Loop (stmt :: st))
                        |= Parser.lazy (\_ -> sExp)
                        |. blank
                    , Parser.backtrackable
                        (Parser.symbol ")"
                            |> Parser.map
                                (\_ ->
                                    st
                                        |> List.foldl
                                            (\x acc -> Pair x acc)
                                            (Sybl NIL)
                                        |> Parser.Done
                                )
                        )
                    ]
            )


dotPair : Parser AST
dotPair =
    Parser.succeed Pair
        |. Parser.symbol "("
        |. blank
        |= Parser.lazy (\_ -> sExp)
        |. blank
        |. Parser.symbol "."
        |. blank
        |= Parser.lazy (\_ -> sExp)
        |. blank
        |. Parser.symbol ")"


lambda : Parser AST
lambda =
    Parser.succeed Lambda
        |. Parser.keyword "lambda"
        |. blank
        |= list
        |. blank
        |= Parser.lazy (\_ -> sExp)


let_ : Parser AST
let_ =
    Parser.succeed Let
        |. Parser.keyword "let"
        |. blank
        |= list
        |. blank
        |= Parser.loop
            []
            (\st ->
                Parser.oneOf
                    [ Parser.succeed (\stmt -> Parser.Loop <| stmt :: st)
                        |= sExp
                        |. blank
                    , Parser.succeed ()
                        |> Parser.map
                            (\_ -> Parser.Done <| List.reverse st)
                    ]
            )


if_ : Parser AST
if_ =
    Parser.succeed If
        |. Parser.keyword "if"
        |. blank
        |= Parser.lazy (\_ -> sExp)
        |. blank
        |= Parser.lazy (\_ -> sExp)
        |. blank
        |= Parser.lazy (\_ -> sExp)


define : Parser AST
define =
    Parser.succeed Define
        |. Parser.keyword "define"
        |. blank
        |= (atom
                |> Parser.andThen
                    (\x ->
                        case x of
                            Label _ ->
                                Parser.succeed (Sybl x)

                            _ ->
                                Parser.problem "define name is only string"
                    )
           )
        |. blank
        |= list
        |. blank
        |= Parser.lazy (\_ -> sExp)


cond : Parser AST
cond =
    Parser.succeed (\( branch, else_ ) -> Cond branch else_)
        |. Parser.keyword "cond"
        |. blank
        |= (condBranch
                |> Parser.andThen
                    (\revBranch ->
                        case ( List.head revBranch, List.tail revBranch ) of
                            ( Just head, Just tail ) ->
                                if elseCheck head then
                                    case cdr_ head |> Maybe.andThen car_ of
                                        Just else_ ->
                                            Parser.succeed ( List.reverse tail, else_ )

                                        _ ->
                                            Parser.problem "error"

                                else
                                    Parser.problem "error"

                            _ ->
                                Parser.problem "error"
                    )
           )


condBranch : Parser (List AST)
condBranch =
    Parser.loop
        []
        (\st ->
            Parser.oneOf
                [ Parser.succeed (\stmt -> Parser.Loop <| stmt :: st)
                    |= list
                    |. blank
                , Parser.succeed ()
                    |> Parser.map
                        (\_ -> Parser.Done st)
                ]
        )


elseCheck : AST -> Bool
elseCheck ast =
    case ast of
        Pair (Sybl (Label "else")) _ ->
            True

        _ ->
            False


blank : Parser ()
blank =
    Parser.loop 0 <|
        ifProgress <|
            oneOf
                [ comment
                , Parser.spaces
                ]


comment : Parser ()
comment =
    Parser.lineComment ";;"


ifProgress : Parser a -> Int -> Parser (Parser.Step Int ())
ifProgress p offset =
    Parser.succeed identity
        |. p
        |= Parser.getOffset
        |> Parser.map
            (\newOffset ->
                if offset == newOffset then
                    Parser.Done ()

                else
                    Parser.Loop newOffset
            )
