module State

type State<'m, 'a> = 'm -> 'a * 'm

let ret (x: 'a) = fun m -> x,m

let (>>=) (p:State<'m,'a>) (k:'a->State<'m,'b>) : State<'m, 'b> =
  fun (m0:'m) ->
    let x,m1 = p m0
    let y,m2 = k x m1
    y,m2

let getMemory = fun m -> m,m
let setMemory m' = fun m -> (),m'

type StateBuilder() =
  member this.Return x = ret x
  member this.Bind(p,k) = p >>= k
let st = StateBuilder()