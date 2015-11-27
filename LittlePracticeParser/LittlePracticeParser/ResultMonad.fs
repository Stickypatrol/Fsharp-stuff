module ResultMonad

type Result<'a> =
  | Result of 'a
  | Error of List<string>

let (>>=) p f =
  match p with
  | Result p -> f p
  | Error x -> Error x

type ResultBuilder() =
  member this.Return a = Result(a)
  member this.Bind p f = p >>= f
let res = ResultBuilder()