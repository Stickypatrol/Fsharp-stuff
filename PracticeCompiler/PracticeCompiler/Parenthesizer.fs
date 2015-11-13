module Parenthesizer
open ParserMonad
open Common
open Lexer

type ParenthesizedExpression = 
  Literal of Literal | Id of Id | Keyword of Keyword
  |Block of List<ParenthesizedExpression>

let literal : Parser<Lexer.Token, ParenthesizedExpression> =
  prs{
    let! h = getFirstSymbol
    match h with 
    | Token.Literal(l) ->
      return ParenthesizedExpression.Literal(l)
    | _ ->
      return! fail ["Error: expected literal."]
  }

let identifier : Parser<Lexer.Token, ParenthesizedExpression> =
  prs{
    let! h = getFirstSymbol
    match h with 
    | Token.Id(i) ->
      return ParenthesizedExpression.Id(i)
    | _ ->
      return! fail ["Error: expected Id."]
  }

let keyword : Parser<Lexer.Token, ParenthesizedExpression> =
  prs{
    let! h = getFirstSymbol
    match h with 
    | Token.Keyword(k) ->
      return ParenthesizedExpression.Keyword(k)
    | _ ->
      return! fail ["Error: expected keyword."]
  }

let openBracket b : Parser<Lexer.Token, Unit> =
  prs{
  let! h = getFirstSymbol
  match h with
  | Token.Open(b') when b = b' ->
    return()
  | _ -> 
    return! fail ["Not a keyword"]
  }

let closedBracket b : Parser<Lexer.Token, Unit> =
  prs{
  let! h = getFirstSymbol
  match h with
  | Token.Closed(b') when b = b' ->
    return()
  | _ -> 
    return! fail ["Not a keyword"]
  }

let termination : Parser<Token, List<ParenthesizedExpression>> =
  prs{
    do! eof .|| (lookahead(closedBracket Bracket.Curly .|| closedBracket Bracket.Round))
    return []
  }
let rec block b : Parser<Token, List<ParenthesizedExpression>> =
  prs{
    do! openBracket b
    let! inner = parenthesizedExpressions()
    do! closedBracket b
    let! outer = parenthesizedExpressions()
    return Block(inner) :: outer
  }

and parenthesizedExpressions() =
  block Bracket.Curly
  .|| block Bracket.Round
  .|| termination
  .||
  prs{
    let! first = keyword .|| literal .|| identifier
    let! rest = parenthesizedExpressions()
    return first :: rest
  }