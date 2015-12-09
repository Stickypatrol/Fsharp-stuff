module ParserMonad

open ResultMonad

type Parser<'char, 'a> = list<'char> -> Result<list<'char> * 'a>

let ret a =
  fun s ->
    res{
      return (s, a)
    }

let (>>=) (p : Parser<'char, 'a>) (f : 'a -> Parser<'char, 'b>) : Parser<'char, 'b> =
  fun (s : List<'char>) ->
    res{
      let! s', a = p s
      return! f a s'
    }

type ParserBuilder() =
  member this.Bind (p, f) = p >>= f
  member this.Return x = ret x
  member this.ReturnFrom p = p

let prs = ParserBuilder()
let (<|>) p1 p2 =
  fun buf ->
    match p1 buf with
    | Result x -> Result x
    | Error e1 ->   match p2 buf with
                    | Result x -> Result x
                    | Error e2 -> Error(List.append e1 e2)

let rec repeat (p:Parser<'char, 'a>) =
  prs{
    let! h = p
    let! t = repeat p
    return (h)
  } <|>
  prs{
    return []
  }

let repeatAtLeastOnce (p:Parser<'char, 'a list>) =
  prs{
    let! h = p
    let! t = repeat p
    return (h::t)
  }