﻿module ErrorMonad

type Result<'a> = Result of 'a | Error of List<string>

//return op
let ret (x:'a) : Result<'a> = Result(x)

//bind op : M 'a -> ('a -> M 'b) -> M 'b
let (>>=) (p:Result<'a>) (k:'a-> Result<'b>) : Result<'b> =
  match p with
  | Error msg -> Error msg
  | Result x -> k x

let fail msg = Error(msg)

type ErrorBuilder() =
  member this.ReturnFrom (p: Result<'a>) = p
  member this.Return(x) = ret x
  member this.Bind(p,k) = p >>= k

let err = ErrorBuilder()




(*
let MonadStuff x y : Result<int> =
  err{
  let! x_v = x
  let! y_v = y
  if x_v <> 0 then
    return x_v/y_v
  else
    return! fail ["error motherfucker, it's undefined "]
  }*)