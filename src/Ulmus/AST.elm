module Ulmus.AST exposing (..)


type AST
    = Sybl Atom
    | Quote AST
    | Pair AST AST
    | Lambda AST AST
    | Let AST (List AST)
    | If AST AST AST


type Atom
    = NIL
    | T
    | Num Float
    | Str String
    | Label String


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

        If cond t f ->
            "if" ++ " " ++ show cond ++ " " ++ show t ++ " " ++ show f


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


equal : AST -> AST -> Bool
equal c1 c2 =
    case ( c1, c2 ) of
        ( Sybl atom1, Sybl atom2 ) ->
            atom1 == atom2

        _ ->
            False
