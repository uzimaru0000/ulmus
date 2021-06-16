module Ulmus exposing (..)

import Dict exposing (Dict)
import Utils


type AST
    = Sybl Atom
    | Quote AST
    | Pair AST AST
    | Lambda AST AST
    | Let AST (List AST)


type Atom
    = NIL
    | T
    | Num Float
    | Str String
    | Label String


type alias Ctx =
    Dict String AST


show : AST -> String
show cell =
    case cell of
        Sybl NIL ->
            "()"

        Sybl T ->
            "t"

        Sybl (Num n) ->
            String.fromFloat n

        Sybl (Str str) ->
            "\"" ++ str ++ "\""

        Sybl (Label str) ->
            String.toUpper str

        Pair c1 c2 ->
            "(" ++ show c1 ++ " " ++ show c2 ++ ")"

        Lambda args body ->
            "lambda " ++ show args ++ " " ++ show body

        Quote ast ->
            "'" ++ show ast

        Let vars body ->
            "let" ++ show vars ++ (body |> List.map show |> String.join " ")


equal : AST -> AST -> Bool
equal c1 c2 =
    case ( c1, c2 ) of
        ( Sybl atom1, Sybl atom2 ) ->
            atom1 == atom2

        _ ->
            False


list_ : List AST -> AST
list_ =
    List.foldr
        (\x acc -> Pair x acc)
        (Sybl NIL)


toList : AST -> List AST
toList ast =
    case ast of
        Pair f s ->
            f :: toList s

        _ ->
            []


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
                                buildin ctx_ (String.toUpper name) x
                            )

        Lambda args body ->
            eval_ ctx snd
                |> Result.andThen
                    (\( x, ctx_ ) ->
                        lambda ctx_ args body x
                    )
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


lambda : Ctx -> AST -> AST -> AST -> Result String ( AST, Ctx )
lambda ctx args body vals =
    if isList args && isList vals && len_ args == len_ vals then
        List.map2 Tuple.pair (toList args) (toList vals)
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


buildin : Ctx -> String -> AST -> Result String ( AST, Ctx )
buildin ctx name args =
    (case name of
        "CAR" ->
            car args

        "CDR" ->
            cdr args

        "CONS" ->
            cons args

        "EQ" ->
            eq args

        "NOT" ->
            not args

        "LIST" ->
            list ctx args

        _ ->
            Err (name ++ " is not found")
    )
        |> Result.map (\x -> ( x, ctx ))


car : AST -> Result String AST
car args =
    if len_ args == 1 then
        case car_ args of
            Just (Pair f _) ->
                Ok f

            _ ->
                Err ("Error: " ++ show args ++ " is not a list")

    else
        Err "Error: CAR take just 1 argument"


cdr : AST -> Result String AST
cdr args =
    if len_ args == 1 then
        case car_ args of
            Just (Pair _ s) ->
                Ok s

            _ ->
                Err ("Error: " ++ show args ++ " is not a list")

    else
        Err "Error: CDR take just 1 argument"


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
            Just (Sybl NIL) ->
                Ok <| Sybl T

            Just _ ->
                Ok <| Sybl NIL

            _ ->
                Err "err"

    else
        Err "err"


list : Ctx -> AST -> Result String AST
list ctx args =
    eval_ ctx args
        |> Result.map Tuple.first
