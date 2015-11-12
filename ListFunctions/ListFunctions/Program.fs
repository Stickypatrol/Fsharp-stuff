module ListModule =
  let Nthfunc (xs : 'a list) (x : int) =
    let rec Nth (xs : 'a list) (x : int) =
      match xs with
      | [] -> failwith "list too short or empty"
      | h::t when x > 0 -> Nth t (x-1)
      | h::t -> h
    Nth xs x

  let Reversefunc (xs : 'a list) =
    let rec Reverse (xs : 'a list) (ys : 'a list) =
      match xs with
      | [] -> ys
      | h::t -> Reverse t (h::ys)
    Reverse xs []

  let Lengthfunc (xs : 'a list) =
    let rec Length (xs : 'a list) (y : int) =
      match xs with
      | [] -> y
      | h::t -> Length t (y + 1)
    Length xs 0

  let Mapfunc (xs : 'a list) (f : 'a -> 'b) =
    let rec Map (xs : 'a list) (ys : 'b list) (f : 'a -> 'b) =
      match xs with
      | [] -> Reversefunc ys
      | h::t -> Map t ((f h)::ys) f
    Map xs [] f

  let Filterfunc (xs : 'a list) (pr : 'a -> bool) =
    let rec Filter (xs : 'a list) (ys : 'a list) (pr : 'a -> bool) =
      match xs with
      | [] -> Reversefunc ys
      | h::t when pr h -> Filter t (h::ys) pr
      | h::t -> Filter t ys pr
    Filter xs [] pr
  
  let rec Foldfunc (xs : 'a list) (acc : 'b) (f : 'b -> ('a * 'a list) -> 'b) =
    match xs with
    | [] -> acc
    | h::t -> Foldfunc t (f acc (h,t)) f


let examplefunc x y = 

let practicemonad x y =
  