module Prioritizer

open Parenthesizer
open Common

type Expression =
  Literal of Literal | Id of Id
  | DividedBy of Expression * Expression
  | Plus of Expression * Expression
  | Minus of Expression * Expression
  | GreaterThan of Expression * Expression
  | Equals of Expression * Expression
  | Print of Expression
  | While of Expression * Expression
  | If of Expression * Expression
  | Semicolon of Expression * Expression

type PartiallyProcessedExpression = 
  | Processed of Expression
  | Unprocessed of ParenthesizedExpression

let rec toPartiallyProcessed (l:List<ParenthesizedExpression>) : List<PartiallyProcessedExpression> = 
  match l with
  | [] -> []
  | [ParenthesizedExpression.Keyword(Keyword.Semicolon)] -> []
  | x :: xs -> PartiallyProcessedExpression.Unprocessed x :: toPartiallyProcessed xs

let rec (!) (p:PartiallyProcessedExpression) : Expression =
  match p with 
  | Processed e -> e
  | Unprocessed u ->
    prioritize [u]

and groupBinaryOperator op expr (l:List<PartiallyProcessedExpression>) : List<PartiallyProcessedExpression> =
  match l with
  | x:: Unprocessed(Keyword(op')) :: y :: xs when op = op' ->
    groupBinaryOperator op expr (Processed(expr(!x, !y)) :: xs)
  | x :: xs -> x :: (groupBinaryFunction op expr xs)
  | [] -> []

and groupBinaryFunction f expr (l:List<PartiallyProcessedExpression>) : List<PartiallyProcessedExpression> =
  match l with
  | Unprocessed(Keyword(f')) :: x :: y :: xs when f = f' ->
    groupBinaryFunction f expr (Processed(expr(!x, !y)) :: xs)
  | x :: xs -> x :: (groupBinaryFunction f expr xs)
  | [] -> []

and groupWhile = groupBinaryFunction Keyword.While Expression.While
and groupIf = groupBinaryFunction Keyword.If Expression.If

and groupUnaryFunction f expr (l:List<PartiallyProcessedExpression>) : List<PartiallyProcessedExpression> =
  match l with
  | Unprocessed(Keyword(f')) :: x :: xs when f = f' ->
    groupUnaryFunction f expr (Processed(expr(!x)) :: xs)
  | x :: xs -> x:: (groupUnaryFunction f expr xs)
  | [] -> []


and groupDivisions = groupBinaryOperator Keyword.DividedBy Expression.DividedBy
and groupSums = groupBinaryOperator Keyword.Plus Expression.Plus
and groupDifferences = groupBinaryOperator Keyword.Minus Expression.Minus
and groupGreaterThan = groupBinaryOperator Keyword.GreaterThan Expression.GreaterThan
and groupEqual = groupBinaryOperator Keyword.Equals Expression.Equals
and groupPrint = groupUnaryFunction Keyword.Print Expression.Print
and groupSemicolon = groupBinaryOperator Keyword.Semicolon Expression.Semicolon


and prioritize (l:List<ParenthesizedExpression>) : Expression =
  let l1 = l|> toPartiallyProcessed
  let a = groupDivisions l1
  let b = groupSums a
  let c = groupDifferences b
  let d = groupGreaterThan c
  let e = groupEqual d
  let f = groupWhile e
  let g = groupIf f
  let h = groupPrint g
  let i = groupSemicolon h
  let confirmProcessed =
    i |> List.map (
      fun x ->
        match x with
        | Unprocessed(ParenthesizedExpression.Id(i)) ->
          Expression.Id(i)
        | Unprocessed(ParenthesizedExpression.Literal(l)) ->
          Expression.Literal(l)
        | Processed(p) -> p
        | Unprocessed(Block(b)) ->
          prioritize b
        | _ -> failwith "Should not happen but still... just in case.")
  match confirmProcessed with
  | [e] -> e
  | _ -> failwith "Should not happen either"
