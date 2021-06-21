module Utils exposing (..)

import Parser exposing (Parser)
import Set
import Ulmus.AST exposing (AST(..), Atom(..))


resultZip : Result x a -> Result x b -> Result x ( a, b )
resultZip a b =
    Result.andThen
        (\x ->
            b |> Result.map (\y -> ( x, y ))
        )
        a


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
