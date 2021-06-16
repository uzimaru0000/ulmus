module Utils exposing (..)


resultZip : Result x a -> Result x b -> Result x ( a, b )
resultZip a b =
    Result.andThen
        (\x ->
            b |> Result.map (\y -> ( x, y ))
        )
        a
