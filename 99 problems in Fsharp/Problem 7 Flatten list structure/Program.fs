type Tree<'a> =
| One of 'a
| Many of 'a Tree list

let listA = [One 1;One 2;One 3;Many [Many [One 1;One 2;Many[One 1;One 2;One 3]]]]
let listB = 1::[]
let listC = []
let listD = [1;2;3]::[4;5;6]::[7;8;9]::[]


let Flatten (x : 'a list) =
  let rec FlattenTree (x : 'a list)
  

[<EntryPoint>]
let main argv = 
    printfn "%A" (ReverseList listA)
    printfn "%A" (ReverseList listB)
    printfn "%A" (ReverseList listC)
    printfn "%A" (ReverseList listD)
    0 // return an integer exit code
