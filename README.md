# ulmus

Ulmus is a Lisp implementation made with Elm

```elm
import Parser
import Platform exposing (worker)
import Ulmus
import Ulmus.AST as Ulmus exposing (show)
import Ulmus.Parser exposing (parser)

eval : String -> Result String AST
eval code =
    Parser.run parser code
        |> Result.mapError (always "error")
        |> Result.andThen (Ulmus.evalAll (Sybl NIL))
        |> Result.map Tuple.first

-- Value
eval "3" --> Ok (Sybl (Num 3))
eval "3.14" --> Ok (Sybl (Num 3.14))
eval "\"Hello\"" --> Ok (Sybl (Str "Hello"))
eval "nil" --> Ok (Sybl NIL)
eval "t" --> Ok (Sybl T)

-- Calculate
eval "(+ 1 2)" --> Ok (Sybl (Num 3))
eval "(+ 1 (* 1 2))" --> Ok (Sybl (Num 3))

-- Lambda
eval "((lambda (x) (+ x 1)) 10)" --> Ok (Sybl (Num 11))

-- If
eval "((lambda (x) (if (> x 10) x (* x 10))) 3)" --> Ok (Sybl (Num 30))

-- Cond
eval """
((lambda (x)
    (cond
      ((eq (mod x 15) 0) "fizzbuzz")
      ((eq (mod x 3) 0) "fizz")
      ((eq (mod x 5) 0) "buzz")
      (else x)
    )
 )
 15
)
""" --> Ok (Sybl (Str "fizzbuzz"))

-- Define function
eval """
(define fact (x)
  (if (<= x 1)
    x
    (* x (fact (- x 1)))
  )
)

(fact 10)
""" --> Ok (Sybl (Num 3628800))

-- comment
eval """
;; this line is comment
(+ 1 2)
"""
```

## Modules

### `Ulmus`

Core module

### `Ulmus.AST`

AST module

### `Ulmus.Parser`

Parser module

### `Ulmus.BuildIn`

BuildIn functions
