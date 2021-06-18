module Ulmus exposing (..)

import Dict exposing (Dict)
import Ulmus.AST exposing (AST(..), Atom(..), equal, show, toList)
import Ulmus.BuildIn exposing (buildIn)
import Utils


type alias Ctx =
    Dict String AST


car_ : AST -> Maybe AST
car_ a =
    case a of
        Pair f _ ->
            Just f

        _ ->
            Nothing


cdr_ : AST -> Maybe AST
cdr_ a =
    case a of
        Pair _ s ->
            Just s

        _ ->
            Nothing


len_ : AST -> Int
len_ ast =
    case ast of
        Pair _ s ->
            1 + len_ s

        Sybl NIL ->
            0

        _ ->
            1


isList : AST -> Bool
isList ast =
    case ast of
        Sybl NIL ->
            True

        Pair _ s ->
            True && isList s

        _ ->
            False


eval : AST -> Result String AST
eval e =
    eval_ (Dict.fromList []) e
        |> Result.map Tuple.first


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
            Dict.get l ctx
                |> Maybe.map (\x -> ( x, ctx ))
                |> Result.fromMaybe (l ++ " is undefined")

        Quote ast ->
            Ok ( ast, ctx )

        Lambda args body ->
            Ok ( Lambda args body, ctx )

        Let _ _ ->
            Err "LET has no value"

        If _ _ _ ->
            Err "IF has no value"

        Pair head tail ->
            evalPair ctx head tail


evalPair : Ctx -> AST -> AST -> Result String ( AST, Ctx )
evalPair ctx fst snd =
    case fst of
        Sybl (Label name) ->
            case eval_ ctx fst of
                Ok ( x, ctx_ ) ->
                    eval_ ctx_ (Pair x snd)

                _ ->
                    eval_ ctx snd
                        |> Result.andThen
                            (\( x, ctx_ ) ->
                                buildin (String.toUpper name) x
                                    |> Result.map (\x_ -> ( x_, ctx_ ))
                            )

        Lambda args body ->
            eval_ ctx snd
                |> Result.andThen
                    (\( x, ctx_ ) -> apply ctx_ args body x)
                |> Result.map
                    (Tuple.mapSecond (always ctx))

        Let vars body ->
            let_ ctx vars
                |> Result.andThen
                    (\c ->
                        body
                            |> List.map (eval_ c)
                            |> List.foldl (\x acc -> Result.andThen (always x) acc) (Ok ( Sybl NIL, c ))
                    )

        If cond t f ->
            eval_ ctx cond
                |> Result.andThen
                    (\( cond_, _ ) ->
                        if equal (Sybl NIL) cond_ then
                            eval_ ctx f

                        else
                            eval_ ctx t
                    )

        _ ->
            Utils.resultZip (eval_ ctx fst) (eval_ ctx snd)
                |> Result.map
                    (\( ( x, ctx1 ), ( y, ctx2 ) ) ->
                        ( Pair x y
                        , Dict.merge
                            (\key a -> Dict.insert key a)
                            (\key a _ -> Dict.insert key a)
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
            Err "err"


apply : Ctx -> AST -> AST -> AST -> Result String ( AST, Ctx )
apply ctx args body vals =
    let
        argsList =
            toList args

        valsList =
            toList vals
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
            |> Result.andThen
                (\c ->
                    eval_ c body
                )

    else
        Err "Error: lambda"


let_ : Ctx -> AST -> Result String Ctx
let_ ctx vars =
    let
        key =
            car_ vars |> Maybe.andThen car_

        val =
            car_ vars |> Maybe.andThen cdr_ |> Maybe.andThen car_
    in
    case ( key, val ) of
        ( Just (Sybl (Label k)), Just v ) ->
            eval_ ctx v
                |> Result.map
                    (\( v_, _ ) -> Dict.insert k v_ ctx)

        _ ->
            Err "err"


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
                Err "err"

    else
        Err "err"


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
    Debug.todo "impl add"


mul : AST -> Result String AST
mul args =
    Debug.todo "impl mul"


sub : AST -> Result String AST
sub args =
    Debug.todo "impl sub"


div : AST -> Result String AST
div args =
    Debug.todo "impl div"
