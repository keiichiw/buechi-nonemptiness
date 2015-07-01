open Printf

type expr =
| S0 of string
| F  of string
| D  of (string * string) * string

let print_expr = function
  | S0 s ->
    printf "S0: %s\n" s
  | F  s ->
    printf "F: %s\n" s
  | D  ((q0, a), q1) ->
    printf "%s %s -> %s\n" q0 a q1
