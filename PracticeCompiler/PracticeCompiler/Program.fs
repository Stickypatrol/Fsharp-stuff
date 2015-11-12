open Lexer
open ErrorMonad


[<EntryPoint>]
let main argv = 
    let file  = System.IO.File.ReadAllText "main.txt"
    match lexer() (List.ofSeq file) with
    | Result(tokens, []) ->
      printfn "%A" tokens
    | _ ->
      printfn "oops something went wrong"
    0