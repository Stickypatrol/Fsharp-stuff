let listA = 1::2::3::4::5::[]
let listB = 1::2::3::4::3::2::1::[]
let listC = 1::[]
let listD = [1;2;3]::[4;5;6]::[7;8;9]::[]


let IsPalindrome (x : 'a list) =
  let y = List.rev x
  let len = (((x.Length |> float)/2.0)|>int)
  let rec CheckPalindrome (x : 'a list) (y : 'a list) (len : int) =
    match (x, y) with
      | (_::_, _::_) when len < 1 -> true
      | (xh::xt, yh::yt) when xh = yh -> CheckPalindrome xt yt (len-1)
      | (xh::xt, yh::yt) -> false
      | (_, _) -> false
  CheckPalindrome x y len


[<EntryPoint>]
let main argv = 
    //printfn "%A" (IsPalindrome listA)
    //printfn "%A" (IsPalindrome listB)
    printfn "%A" (IsPalindrome listC)
    printfn "%A" (IsPalindrome listD)
    0 // return an integer exit code
