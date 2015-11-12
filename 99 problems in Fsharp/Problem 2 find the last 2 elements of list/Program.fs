let listA = 1::2::3::4::5::[]
let listB = 1::[]
let listC = []

let rec ReturnLastTwo (xs : int list) =
  match xs with
  | [] ->   printfn "empty list"
            (None, None)
  | h::t when t.Length = 1 -> (Some h, Some t.Head)
  | h::t when t.Length = 0 -> printfn "there is only 1 element in the list"
                              (None, Some h)
  | h::t -> ReturnLastTwo t



[<EntryPoint>]
let main argv = 
    printfn "%A" (ReturnLastTwo listA)
    printfn "%A" (ReturnLastTwo listB)
    printfn "%A" (ReturnLastTwo listC)
    0 // return an integer exit code
