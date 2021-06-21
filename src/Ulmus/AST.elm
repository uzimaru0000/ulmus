module Ulmus.AST exposing
    ( AST(..)
    , Atom(..)
    , equal
    , show
    )

{-| The AST definition of the program

@docs AST
@docs Atom
@docs equal
@docs show

-}


{-| AST
-}
type AST
    = Sybl Atom
    | Quote AST
    | Pair AST AST
    | Lambda AST AST
    | Let AST (List AST)
    | If AST AST AST
    | Define AST AST AST
    | Cond (List AST) AST


{-| Atom
-}
type Atom
    = NIL
    | T
    | Num Float
    | Str String
    | Label String


{-| show
-}
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

        Define name args body ->
            "define " ++ show name ++ " " ++ show args ++ " " ++ show body

        Cond branch else_ ->
            "cond " ++ (List.map show branch |> String.join " ") ++ show else_


{-| equal (Sybl NIL) (Sybl NIL) == True
-}
equal : AST -> AST -> Bool
equal c1 c2 =
    case ( c1, c2 ) of
        ( Sybl atom1, Sybl atom2 ) ->
            atom1 == atom2

        _ ->
            False
