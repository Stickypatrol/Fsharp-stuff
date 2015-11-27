module CommonStuff

type Literal =
  | Int of int
  | Bool of bool

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

type Id = string

type Bracket =
  | Round
  | Curly

type Token = 
  | Literal of Literal
  | Id of Id
  | Keyword of Keyword
  | Open of Bracket
  | Closed of Bracket