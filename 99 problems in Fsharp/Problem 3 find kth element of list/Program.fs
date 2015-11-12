let listA = 1::2::3::4::5::[]
let listB = 1::[]
let listC = []



let rec ReturnKthElement (xs : int list) (i : int) =
  match xs with
  | [] -> failwith "there are no elements in this list or the index is outside the range of the list"
  | h::t when i > 0 -> ReturnKthElement t (i-1)
  | h::t -> h






[<EntryPoint>]
let main argv = 
    printfn "%A" (ReturnKthElement listA 1)
    printfn "%A" (ReturnKthElement listA 3)
    printfn "%A" (ReturnKthElement listA 4)
    printfn "%A" (ReturnKthElement listA 7)
    printfn "%A" (ReturnKthElement listB 0)
    printfn "%A" (ReturnKthElement listC 0)
    0 // return an integer exit code
