port module Main exposing (..)

import Dict exposing (Dict)
import Parser
import Platform exposing (worker)
import Ulmus
import Ulmus.AST as Ulmus exposing (show)
import Ulmus.Parser exposing (parser)


type alias Model =
    Ulmus.Ctx


main : Program () Model String
main =
    worker
        { init = init
        , update =
            \code ctx ->
                let
                    result =
                        run ctx code
                in
                case result of
                    Ok ( result_, ctx_ ) ->
                        ( ctx_, show result_ |> output )

                    Err err ->
                        ( ctx, output err )
        , subscriptions = \_ -> input identity
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Dict.fromList []
    , Cmd.none
    )


run : Ulmus.Ctx -> String -> Result String ( Ulmus.AST, Ulmus.Ctx )
run ctx code =
    Parser.run parser code
        |> Result.mapError (always "error")
        |> Result.andThen (Ulmus.evalAll ctx)


port output : String -> Cmd msg


port input : (String -> msg) -> Sub msg
