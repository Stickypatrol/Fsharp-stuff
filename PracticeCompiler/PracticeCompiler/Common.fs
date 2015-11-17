module Common

type Literal = Int of int | Bool of bool
type Id = string
type Bracket = Round | Curly
type Keyword =
      | While
      | If
      | Equals
      | Semicolon
      | Minus
      | Plus
      | DividedBy
      | Print
      | GreaterThan