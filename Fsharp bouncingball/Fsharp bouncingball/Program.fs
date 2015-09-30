// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Math = 
    [<Measure>]
    type m
    [<Measure>]
    type kg
    [<Measure>]
    type s
    [<Measure>]
    type N = kg*m/s^2
   
    type Vector2<[<Measure>] 'a> = {X: float<'a>; Y: float<'a>} 
        with
        static member Zero : Vector2<'a> = { X = 0.0<_>; Y = 0.0<_> }
        static member (+) (v1:Vector2<'a>, v2:Vector2<'a>):Vector2<'a> = {X = v1.X + v2.X; Y = v1.Y + v2.Y}
        static member (+) (v:Vector2<'a>, k:float<'a>):Vector2<'a> =
            {X = v.X + k; Y = v.Y + k}
        static member (+) (k:float<'a>,v:Vector2<'a>):Vector2<'a> =
            v + k
        static member (~-) (v: Vector2<'a>):Vector2<'a> = 
           {X = -v.X;Y = -v.Y}
        static member (-) (v1:Vector2<'a>, v2:Vector2<'a>):Vector2<'a> =
            {X = v1.X - v2.X; Y = v1.Y - v2.Y}
        static member (-) (v:Vector2<'a>, k:float<'a>):Vector2<'a> =
            v+(-k)
        static member (-) (k:float<'a>,v:Vector2<'a>):Vector2<'a> =
            k+(-v)
        static member (*) (v1:Vector2<'a>, v2:Vector2<'b>):Vector2<'a*'b> =
            {X = v1.X * v2.X; Y = v1.Y * v2.Y}
        static member (*) (v:Vector2<'a>, k:float<'b>):Vector2<'a * 'b> =
            {X = v.X * k; Y = v.Y * k}
        static member (*) (k:float<'b>,v:Vector2<'a>):Vector2<'a> =
            v * k
        member this.Length : float<'a> = 
            sqrt((this.X*this.X+this.Y*this.Y))
        static member Distance(v1:Vector2<'a>, v2:Vector2<'a>) =
            (v1-v2).Length
        static member Normalize(v:Vector2<'a>) :Vector2<1> =
            {X = v.X/v.Length; Y = v.Y/v.Length}

open System
open Math


let dt = 0.016<s>
let g = {X = 0.0<m/s^2>;Y = -9.81<m/s^2>}

let bounds_check (p: Vector2<m> , v: Vector2<m/s>) =
    match (p, v) with
    |(p, v) when p.X < 0.0<m> -> ({X = 0.0<m> ; Y = p.Y}, {X = -v.X*0.7 ; Y = v.Y * 0.7})
    |(p, v) when p.X > 28.0<m> -> ({X = 28.0<m> ; Y = p.Y}, {X = -v.X*0.7 ; Y = v.Y * 0.7})
    |(p, v) when p.Y < 0.0<m> -> ({X = p.X ; Y = 0.0<m>}, {X = v.X * 0.7 ; Y = -v.Y*0.7})
    |(p, v) when p.Y > 8.0<m> -> ({X = p.X ; Y = 8.0<m>}, {X = v.X  * 0.7; Y = -v.Y*0.7})
    | _ -> (p, v)


let simulation_step (listA: list<Vector2<m>*Vector2<m/s>>) =
    let listA' = listA
                |> List.map (fun (p, v) -> (p + v * dt, v + g * dt))//indentation fixed this
                |> List.map bounds_check
    listA'

//let check_ball (listA: list<Vector2<m> * Vector2<m/s>>) =a
//this is a possible function to find out if a certain ball matches with the location  


let print_scene (listA: list<Vector2<m>*Vector2<m/s>>) =
    do Console.Clear()
    let mutable buffer = ""
    for j = 10 downto 0 do
        for i = 0 to 30 do
            if List.exists (fun (p:Vector2<m>, v:Vector2<m/s>) -> round <| float p.Y + 1.0 = (float j) && round <| float p.X + 1.0 = (float i)) listA then
                do buffer <- buffer + ("O")
            elif j = 0 || i = 0 || j = 10 || i = 30 then
                do buffer <- buffer + ("X")
            else
                do buffer <- buffer + (" ")
        do buffer <- buffer + "\n"
    do Console.Write(buffer)
    do System.Threading.Thread.Sleep(16) // ignore(Console.ReadKey())

let rec simulation (listA) =
    do print_scene(listA)
    let listA' = simulation_step (listA)
    if List.forall (fun (p: Vector2<m>, v: Vector2<m/s>) -> p.Y > 0.1<m> || abs v.X > 0.9<m/s> || abs v.Y > 0.9<m/s>) listA' then
        do simulation (listA')
    else
        exit(0)

let listA = [({X = 13.0<m>; Y = 5.0<m>},{X = -7.0<m/s>; Y = 5.0<m/s>});({X = 15.0<m>; Y = 7.0<m>},{X = 7.0<m/s>; Y = 2.0<m/s>})]

do simulation(listA)