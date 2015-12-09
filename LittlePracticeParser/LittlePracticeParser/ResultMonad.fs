module ResultMonad

type Result<'a> =
  | Result of 'a
  | Error of List<string>

let (>>=) p f =
  match p with
  | Result x -> f x
  | Error x -> Error x

let fail err = Error err

type ResultBuilder() =
  member this.ReturnFrom p = p
  member this.Return a = Result(a)
  member this.Bind (p, f) = p >>= f
let res = ResultBuilder()