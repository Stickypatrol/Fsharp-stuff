module ParserMonad

open ErrorMonad

type Parser<'char, 'a> = List<'char> -> Result<'a * List<'char>>

let ret (x:'a) : Parser<'char, 'a> =
  fun (buf:List<'char>) ->
    err{  
      return (x, buf)
    } : Result<'a * List<'char>>

let fail (msg : List<string>) : Parser<'char, 'a> =
  fun buf -> ErrorMonad.fail msg

let (>>=) (p:Parser<'char,'a>) (k:'a-> Parser<'char,'b>) : Parser<'char,'b> =
  fun (buf:List<'char>) ->
    err{
      let! x,buf' = p buf
      return! k x buf'
    } : Result<'b * List<'char>>
  
type ParserBuilder() =
  member this.ReturnFrom(p) = p
  member this.Return(x) = ret x
  member this.Bind(p,k) = p >>= k
  member this.For(s:seq<'a>, k:'a->Parser<'char,Unit>) : Parser<'char,Unit> =
    if s |> Seq.isEmpty then
      ret ()
    else
      this.Bind(k(s |> Seq.head), fun () -> this.For(s |> Seq.tail, k))
let prs = ParserBuilder()

let getFirstSymbol : Parser<'char,'char> =
  fun buf ->
    match buf with
    | x::xs -> 
      err{
        return(x,xs)
      }
    | [] ->
      err{
        return! ErrorMonad.fail ["unexpected EOF"]
      }

let eof : Parser<'char, unit> =
  fun buf ->
    match buf with
    | x::xs -> 
      err{
        return! ErrorMonad.fail ["unexpected symbol"]
      }
    | [] ->
      err{
        return ((),[])
      }

let (.||) (p1:Parser<'char, 'a>) (p2:Parser<'char, 'a>) : Parser<'char,'a> =
  fun buf ->
    match p1 buf with
    | Error e1 ->
      match p2 buf with
      | Error e2 ->
        Error(List.append e1 e2)
      | Result(x, buf') ->
        Result(x, buf')
    | Result(x, buf') ->
      Result(x, buf')

let lookahead (p:Parser<'char, 'a>) : Parser<'char, Unit> =
  fun buf ->
    match p buf with
    | Error msg -> Error msg
    | Result(x,buf') -> Result((), buf)



let rec repeat (p: Parser<'char, 'a>) : Parser<'char,List<'a>> =
  prs{
    let! x = p
    let! xs = repeat p
    return x::xs
  } .||
  prs{
    return []
  }

let rec repeatAtLeastOnce (p: Parser<'char, 'a>) : Parser<'char,List<'a>> =
  prs{
    let! x = p
    let! xs = repeat p
    return x::xs
  }