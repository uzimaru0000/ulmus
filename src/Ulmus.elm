module Ulmus exposing (..)

import Dict exposing (Dict)
import Ulmus.AST exposing (AST(..), Atom(..), car_, cdr_, equal, len_, list_, show, toList)
import Ulmus.BuildIn exposing (buildIn)
import Utils


type alias Ctx =
    Dict String AST


eval : Ctx -> AST -> Result String AST
eval ctx e =
    evalAll ctx buildIn
        |> Result.map Tuple.second
        |> Result.andThen (\ctx_ -> eval_ ctx_ e)
        |> Result.map Tuple.first


evalAll : Ctx -> List AST -> Result String ( AST, Ctx )
evalAll ctx e =
    e
        |> List.foldl
            (\x acc ->
                acc
                    |> Result.map Tuple.second
                    |> Result.andThen (\ctx_ -> eval_ ctx_ x)
            )
            (Ok ( Sybl NIL, ctx ))


eval_ : Ctx -> AST -> Result String ( AST, Ctx )
eval_ ctx e =
    case e of
        Sybl T ->
            Ok ( Sybl T, ctx )

        Sybl NIL ->
            Ok ( Sybl NIL, ctx )

        Sybl (Num n) ->
            Ok ( Sybl (Num n), ctx )

        Sybl (Str s) ->
            Ok ( Sybl (Str s), ctx )

        Sybl (Label l) ->
            ctx
                |> Dict.get l
                |> Maybe.map (\x -> ( x, ctx ))
                |> Result.fromMaybe (l ++ " is undefined")

        Quote ast ->
            Ok ( ast, ctx )

        Lambda _ _ ->
            Ok ( e, ctx )

        Let _ _ ->
            Err "LET has no value"

        If _ _ _ ->
            Err "IF has no value"

        Cond _ _ ->
            Err "COND has no value"

        Define name args body ->
            define ctx name args body

        Pair head tail ->
            evalPair ctx head tail


evalPair : Ctx -> AST -> AST -> Result String ( AST, Ctx )
evalPair ctx fst snd =
    case fst of
        Sybl (Label name) ->
            case eval_ ctx fst of
                Ok ( x, ctx_ ) ->
                    eval_ ctx_ (Pair x snd)

                Err _ ->
                    case specialForm (String.toUpper name) ctx snd of
                        Ok res ->
                            Ok res

                        Err _ ->
                            eval_ ctx snd
                                |> Result.andThen
                                    (\( x, ctx_ ) ->
                                        buildin (String.toUpper name) x
                                            |> Result.map (\x_ -> ( x_, ctx_ ))
                                    )

        Lambda args body ->
            apply ctx args body snd
                |> Result.map (Tuple.mapSecond (always ctx))

        Let vars body ->
            let_ ctx vars
                |> Result.andThen (\c -> letEval c body)

        If cond t f ->
            cond
                |> eval_ ctx
                |> Result.andThen
                    (\( cond_, _ ) ->
                        if equal (Sybl NIL) cond_ then
                            eval_ ctx f

                        else
                            eval_ ctx t
                    )

        Cond branch else_ ->
            evalCond ctx branch else_

        Pair (Lambda args body) _ ->
            apply ctx args body snd
                |> Result.map (Tuple.mapSecond (always ctx))

        _ ->
            Utils.resultZip (eval_ ctx fst) (eval_ ctx snd)
                |> Result.map
                    (\( ( x, ctx1 ), ( y, ctx2 ) ) ->
                        ( Pair x y
                        , Dict.merge
                            (\key a -> Dict.insert key a)
                            (\key _ b -> Dict.insert key b)
                            (\key b -> Dict.insert key b)
                            ctx1
                            ctx2
                            Dict.empty
                        )
                    )


bind : Ctx -> AST -> AST -> Result String Ctx
bind ctx key val =
    case key of
        Sybl (Label l) ->
            Dict.insert l val ctx
                |> Ok

        _ ->
            Err "binding error"


apply : Ctx -> AST -> AST -> AST -> Result String ( AST, Ctx )
apply ctx args body vals =
    let
        argsList =
            toList args

        valsList =
            toList vals
                |> List.map (eval_ ctx)
                |> List.foldr
                    (\x acc ->
                        x
                            |> Result.map Tuple.first
                            |> Result.andThen (\v -> Result.map ((::) v) acc)
                    )
                    (Ok [])
                |> Result.withDefault []
    in
    if List.length argsList == List.length valsList then
        List.map2 Tuple.pair argsList valsList
            |> List.foldl
                (\( k, v ) acc ->
                    Result.andThen
                        (\c -> bind c k v)
                        acc
                )
                (Ok ctx)
            |> Result.andThen (\c -> eval_ c body)

    else
        Err "Error: lambda"


define : Ctx -> AST -> AST -> AST -> Result String ( AST, Ctx )
define ctx name args body =
    case name of
        Sybl (Label name_) ->
            Ok <|
                ( Sybl NIL
                , Dict.insert
                    name_
                    (Lambda args body)
                    ctx
                )

        _ ->
            Err "Syntax Error"


let_ : Ctx -> AST -> Result String Ctx
let_ ctx vars =
    case vars of
        Pair head tail ->
            let
                key =
                    car_ head

                val =
                    cdr_ head |> Maybe.andThen car_
            in
            case ( key, val ) of
                ( Just (Sybl (Label k)), Just v ) ->
                    eval_ ctx v
                        |> Result.map
                            (\( v_, _ ) -> Dict.insert k v_ ctx)
                        |> Result.andThen (\c -> let_ c tail)

                _ ->
                    Err "LET: Syntax Error"

        Sybl NIL ->
            Ok ctx

        _ ->
            Err "LET: Syntax Error"


letEval : Ctx -> List AST -> Result String ( AST, Ctx )
letEval ctx ast =
    ast
        |> List.foldl
            (\ast_ acc ->
                case acc of
                    Ok ( _, _ ) ->
                        eval_ ctx ast_

                    Err err ->
                        Err err
            )
            (Ok ( Sybl NIL, ctx ))


evalCond : Ctx -> List AST -> AST -> Result String ( AST, Ctx )
evalCond ctx branch else_ =
    branch
        |> List.foldr
            (\x acc ->
                acc
                    |> Result.andThen
                        (\y ->
                            case ( car_ x, cdr_ x |> Maybe.andThen car_ ) of
                                ( Just head, Just tail ) ->
                                    Ok <| If head tail y

                                _ ->
                                    Err "COND: Syntax error"
                        )
                    |> Result.map List.singleton
                    |> Result.map list_
            )
            (Ok else_)
        |> Result.andThen (eval_ ctx)


specialForm : String -> Ctx -> AST -> Result String ( AST, Ctx )
specialForm name ctx ast =
    case name of
        "OR" ->
            or ctx ast

        "AND" ->
            and ctx ast

        "QUOTE" ->
            car_ ast
                |> Result.fromMaybe "QUOTE: Error"
                |> Result.map (\x -> ( x, ctx ))

        _ ->
            Err <| name ++ " is undefined"


buildin : String -> AST -> Result String AST
buildin name args =
    case name of
        "CAR" ->
            car args

        "CDR" ->
            cdr args

        "CONS" ->
            cons args

        "NOT" ->
            not args

        "EQ" ->
            eq args

        "LIST" ->
            list args

        "+" ->
            add args

        "-" ->
            sub args

        "*" ->
            mul args

        "/" ->
            div args

        "MOD" ->
            mod args

        "<" ->
            compOperation (<) args

        ">" ->
            compOperation (>) args

        "<=" ->
            compOperation (<=) args

        ">=" ->
            compOperation (>=) args

        _ ->
            Err (name ++ " is not found")


car : AST -> Result String AST
car args =
    case args of
        Pair (Pair a _) _ ->
            Ok a

        _ ->
            Err ("Error: " ++ show args ++ " is not a list")


cdr : AST -> Result String AST
cdr args =
    case args of
        Pair (Pair _ a) _ ->
            Ok a

        _ ->
            Err ("Error: " ++ show args ++ " is not a list")


cons : AST -> Result String AST
cons args =
    if len_ args == 2 then
        case ( car_ args, cdr_ args |> Maybe.andThen car_ ) of
            ( Just a, Just b ) ->
                Ok <| Pair a b

            _ ->
                Err "cons: error"

    else
        Err "Error: CONS take just 2 arguments"


eq : AST -> Result String AST
eq args =
    if len_ args == 2 then
        case ( car_ args, cdr_ args |> Maybe.andThen car_ ) of
            ( Just a, Just b ) ->
                Ok <|
                    if equal a b then
                        Sybl T

                    else
                        Sybl NIL

            _ ->
                Err "EQ: Error"

    else
        Err "EQ: Error"


not : AST -> Result String AST
not args =
    if len_ args == 1 then
        case car_ args of
            Just v ->
                if equal v (Sybl NIL) then
                    Ok <| Sybl T

                else
                    Ok <| Sybl NIL

            Nothing ->
                Err "err"

    else
        Err "err"


list : AST -> Result String AST
list args =
    Ok args


add : AST -> Result String AST
add args =
    case args of
        Pair (Sybl (Num a)) tail ->
            add tail
                |> Result.andThen
                    (\x ->
                        case x of
                            Sybl (Num x_) ->
                                Ok <| a + x_

                            _ ->
                                Err "ADD: Not Number"
                    )
                |> Result.map (Sybl << Num)

        Sybl NIL ->
            Ok (Sybl <| Num 0)

        _ ->
            Err "ADD: Syntax Error"


mul : AST -> Result String AST
mul args =
    case args of
        Pair (Sybl (Num a)) tail ->
            mul tail
                |> Result.andThen
                    (\x ->
                        case x of
                            Sybl (Num x_) ->
                                Ok <| a * x_

                            _ ->
                                Err ""
                    )
                |> Result.map (Sybl << Num)

        Sybl NIL ->
            Ok (Sybl <| Num 1)

        _ ->
            Err ""


sub : AST -> Result String AST
sub args =
    let
        helper : AST -> AST -> Result String AST
        helper init v =
            case init of
                Sybl (Num a) ->
                    case v of
                        Pair (Sybl (Num b)) tail ->
                            helper (Sybl <| Num (a - b)) tail

                        Sybl NIL ->
                            Ok (Sybl <| Num a)

                        _ ->
                            Err "SUB: Error"

                _ ->
                    Err "SUB: Error"
    in
    case args of
        Pair (Sybl (Num a)) (Sybl NIL) ->
            Ok (Sybl <| Num (-1 * a))

        Pair (Sybl (Num a)) tail ->
            helper (Sybl <| Num a) tail

        _ ->
            Err "SUB: Error"


div : AST -> Result String AST
div args =
    let
        helper : AST -> AST -> Result String AST
        helper init v =
            case init of
                Sybl (Num a) ->
                    case v of
                        Pair (Sybl (Num b)) tail ->
                            helper (Sybl <| Num (a / b)) tail

                        Sybl NIL ->
                            Ok (Sybl <| Num a)

                        _ ->
                            Err "DIV: Error"

                _ ->
                    Err "DIV: Error"
    in
    case args of
        Pair (Sybl (Num a)) (Sybl NIL) ->
            Ok (Sybl <| Num (1 / a))

        Pair (Sybl (Num a)) tail ->
            helper (Sybl <| Num a) tail

        _ ->
            Err "DIV: Error"


mod : AST -> Result String AST
mod args =
    case ( car_ args, cdr_ args |> Maybe.andThen car_ ) of
        ( Just (Sybl (Num a)), Just (Sybl (Num b)) ) ->
            Ok (Sybl <| Num <| toFloat (modBy (floor b) (floor a)))

        _ ->
            Err "MOD: Error"


compOperation : (Float -> Float -> Bool) -> AST -> Result String AST
compOperation compFunc args =
    let
        helper init args_ =
            case init of
                Sybl (Num a) ->
                    case args_ of
                        Pair (Sybl (Num b)) tail ->
                            if compFunc a b then
                                helper (Sybl (Num b)) tail

                            else
                                Ok <| Sybl NIL

                        Sybl NIL ->
                            Ok <| Sybl T

                        _ ->
                            Err ""

                _ ->
                    Err ""
    in
    case args of
        Pair (Sybl (Num a)) tail ->
            helper (Sybl (Num a)) tail

        _ ->
            Err ""


or : Ctx -> AST -> Result String ( AST, Ctx )
or ctx args =
    case args of
        Pair head tail ->
            eval_ ctx head
                |> Result.andThen
                    (\( v, ctx_ ) ->
                        if equal v (Sybl NIL) then
                            or ctx_ tail

                        else
                            Ok ( v, ctx_ )
                    )

        Sybl NIL ->
            Ok ( Sybl NIL, ctx )

        _ ->
            Err ""


and : Ctx -> AST -> Result String ( AST, Ctx )
and ctx args =
    let
        helper c init val =
            if equal init (Sybl NIL) then
                Ok ( Sybl NIL, c )

            else
                case val of
                    Pair head tail ->
                        eval_ c head
                            |> Result.andThen
                                (\( v, c_ ) ->
                                    if equal v (Sybl NIL) then
                                        Ok ( Sybl NIL, c_ )

                                    else
                                        helper c_ v tail
                                )

                    Sybl NIL ->
                        Ok ( init, c )

                    _ ->
                        Err ""
    in
    helper ctx (Sybl T) args
