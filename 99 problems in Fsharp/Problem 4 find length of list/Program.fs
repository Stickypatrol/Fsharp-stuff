let listA = 1::2::3::4::5::[]
let listB = 1::[]
let listC = []
let listD = []::[]::[]::[]



let ReturnLength (x : 'a list) =
  let rec Length (x : 'a list) (i : int) =
    match x with
    | [] -> i
    | h::t -> Length t (i+1)
  Length x 0





[<EntryPoint>]
let main argv = 
    printfn "%A" (ReturnLength listA)
    printfn "%A" (ReturnLength listB)
    printfn "%A" (ReturnLength listC)
    printfn "%A" (ReturnLength listD)
    0 // return an integer exit code
