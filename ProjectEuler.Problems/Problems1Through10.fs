module Problems1Through10

open System
open System.Linq
open System.Collections.Generic


#light

let findIfDivisibleByAll (a : int) : bool = 
    if a = 0 then false
    elif a % 19 <> 0 then false
    elif a % 18 <> 0 then false
    elif a % 17 <> 0 then false
    elif a % 16 <> 0 then false
    elif a % 15 <> 0 then false
    elif a % 14 <> 0 then false
    elif a % 13 <> 0 then false
    elif a % 12 <> 0 then false
    elif a % 11 <> 0 then false
    else true

let problemOne unit : int = [0..1000] |> Seq.filter(fun f -> f % 3 = 0 || f % 5 = 0) |> Seq.sum     
  
let problemTwo unit : int = 
    Seq.unfold(fun (current, next) -> Some(current, (next, current + next))) (0, 1) 
    |> Seq.takeWhile(fun f -> f < 4000000) 
    |> Seq.filter(fun f -> f % 2 = 0) 
    |> Seq.sum

let problemThree unit : int64 = Helpers.findAllFactors(600851475143L) |> Seq.max

let problemFour unit = 
    Seq.initInfinite(fun f -> 999 - f) 
    |> Seq.takeWhile(fun f -> f > 99) 
    |> Seq.map(fun f -> 
        Seq.initInfinite(fun v -> 999 - v) 
        |> Seq.takeWhile(fun v -> v > 99) |> Seq.filter(fun m -> Helpers.isPalindrome(m * f)) |> Seq.map(fun m -> f * m)) |> Seq.concat |> Seq.max

let problemFive unit : int = Seq.initInfinite(fun f -> f * 20) |> Seq.find(fun f -> findIfDivisibleByAll(f))

let problemSix unit = 
    Math.Pow([1..100] |> Seq.map(fun f -> Convert.ToDouble(f))
        |> Seq.fold(fun acc elem -> acc + elem) 0.0, 2.0) - ([1..100] |> Seq.map(fun f -> Convert.ToDouble(f)) 
        |> Seq.fold(fun acc elem -> acc + (elem * elem)) 0.0)

let problemEight (a : seq<int>) = 
    a |> Seq.windowed(5) 
      |> Seq.map(fun f -> f |>  Seq.fold(fun acc item -> acc * item) 1) 
      |> Seq.max

let problemNine unit = Helpers.primeSieve(2000000L) |> Seq.sum

let problemTen unit = 
    let mutable result = 0.

    Seq.init 500 (fun f -> Convert.ToDouble(f)) 
    |> Seq.map(fun f -> Seq.init 500 (fun v -> (f, Convert.ToDouble(v)))) |> Seq.concat
    |> Seq.map(fun (k, v) -> 
        let cSquared = k * k + v * v
        let c = Math.Sqrt(cSquared)
        (k, v, c))
    |> Seq.filter(fun (k,v,c) -> k + v + c = 1000.0) 
    |> Seq.map(fun (k,v,c) -> k * v * c) 
    |> Seq.head







