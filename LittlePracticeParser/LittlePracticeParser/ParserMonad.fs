module ParserMonad

open ResultMonad

type Parser<'a, 's> = 's -> 's * 'a

let ret x = fun s -> (x, s)

let (>>=) (p : Parser<'a, 's>) (f : 'a -> Parser<'b, 's>) : Parser<'b, 's> =
  fun s ->
    let s', a = p s
    f a s'

type ParserBuilder() =
  member this.Bind p f = p >>= f
  member this.Return x = fun s -> (s, x)

let prs = ParserBuilder()
let (<|>) p1 p2 =
  fun buf ->
    match p1 buf with
    | Result x -> Result x
    | Error e1 ->   match p2 buf with
                    | Result x -> Result x
                    | Error e2 -> Error(List.append e1 e2)