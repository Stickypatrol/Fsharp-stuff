let listA = 1::2::3::4::5::[]
let listB = 1::[]
let listC = []
let listD = [1;2;3]::[4;5;6]::[7;8;9]::[]


let ReverseList (x : 'a list) =
  let rec Reverse (x : 'a list) (y : 'a list) =
      match x with
      | [] -> y
      | h::t -> Reverse t (h::y)
  Reverse x []

[<EntryPoint>]
let main argv = 
    printfn "%A" (ReverseList listA)
    printfn "%A" (ReverseList listB)
    printfn "%A" (ReverseList listC)
    printfn "%A" (ReverseList listD)
    0 // return an integer exit code
