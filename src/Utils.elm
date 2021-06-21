module Utils exposing (..)

import Parser exposing (Parser)
import Set


resultZip : Result x a -> Result x b -> Result x ( a, b )
resultZip a b =
    Result.andThen
        (\x ->
            b |> Result.map (\y -> ( x, y ))
        )
        a


anyString : Parser String
anyString =
    Parser.variable
        { start = \_ -> True
        , inner = \c -> c /= ' '
        , reserved = Set.fromList []
        }
