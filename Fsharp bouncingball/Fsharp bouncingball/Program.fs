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
//        static member Normalize(v:Vector2<'a>) :Vector2<1> =
//            v/v.Length

open System
open Math


let dt = 0.1<s>
let g = -9.81

let bounds_check (p: Vector2<m> , v: Vector2<m/s>) =
    match (p, v) with
    |(p, v) when p.X < 0.0<m> -> ({X = 0.0<m> ; Y = p.Y}, {X = -v.X*0.7 ; Y = v.Y})
    |(p, v) when p.X > 30.0<m> -> ({X = 30.0<m> ; Y = p.Y}, {X = -v.X*0.7 ; Y = v.Y})
    |(p, v) when p.Y < 0.0<m> -> ({X = p.X ; Y = 0.0<m>}, {X = v.X ; Y = -v.Y*0.7})
    |(p, v) when p.Y > 10.0<m> -> ({X = p.X ; Y = 10.0<m>}, {X = v.X ; Y = -v.Y*0.7})
    | _ -> (p, v)


let simulation_step (listA: list<Vector2<m>*Vector2<m/s>>) =
    let listA' = listA
                |> List.map (fun (p, v) -> (p + v*dt, v))//indentation fixed this
                |> List.map bounds_check
    listA'

//let check_ball (listA: list<Vector2<m> * Vector2<m/s>>) =
//this is a possible function to find out if a certain ball matches with the location  


let print_scene (listA: list<Vector2<m>*Vector2<m/s>>) =
    do Console.Clear()
    for j = 10 downto 0 do
        for i = 0 to 30 do
            if List.exists(fun (p: Vector2<m>, v: Vector2<m/s>) -> round p.Y = (float j)<m> && round p.X = (float i)<m>) listA then
                do Console.Write("O")
            elif j = 0 || i = 0 || j = 10 || i = 30 then
                do Console.Write("X")
            else
                do Console.Write(" ")
        Console.Write("\n")
    do ignore(Console.ReadKey())

let rec simulation (listA)=
    do print_scene(listA)
    let listA' = simulation_step (listA)
    if List.forall (fun (y,v) -> y > 0.0 || abs v > 0.2) listA' then
        do simulation (listA')
    else
        exit(0)

let listA = [({X = 5.0<m>; Y = 13.0<m>};{X = -2.0<m>; Y = 2.0<m>});({X = 7.0<m>; Y = 15.0<m>};{X = -2.0<m>; Y = 2.0<m>})]

do simulation(listA)