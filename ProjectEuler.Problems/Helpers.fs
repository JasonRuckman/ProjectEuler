module Helpers

open System
open System.Collections.Generic
open System.Numerics

let factorial (a : int64) = 
    let mutable fact = BigInteger(1)

    for i in a .. -1L .. 1L do
        fact <- BigInteger.Multiply(fact, BigInteger(i)) 
        
    fact
       
       
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let sqrt_int(x:int) = x |> float |> sqrt |> int
let sqrt_int64(x:int64) = x |> float |> sqrt |> int64

let factor (a : int) = 
    let output = new List<int>()

    for i = 1 to a / 2 do
        if(a % i = 0) then
            output.Add(i)
    
    output
       
let findAllFactors (a : int64) = 
    let mutable stop = false
    let mutable input = a

    let factors = new List<int64>()

    while stop = false do 
        if a % 2L = 0L then 
            factors.Add(2L)
        elif a = 1L then 
            factors.Add(1L)
            stop <- true
        else 
            let mutable f = 3L
            while stop = false do 
                let rem = ref 0L
                let q = Math.DivRem(input, f, rem)
                if !rem = 0L then 
                    factors.Add(f)
                    input <- q
                    if input < (f * f) then 
                        factors.Add(input)
                        stop <- true
                else 
                    f <- f + 2L
    factors

let primeSieve (a : int64) = 
    let knownComposites = new HashSet<int64>()

    for i in 3L .. 2L .. a do
       for j in 2L * i .. i .. a do
        if knownComposites.Contains(j) = false then ignore(knownComposites.Add(j))

    let intermediate = seq { for i in 3L .. 2L .. a -> i } |> Seq.filter(fun f -> knownComposites.Contains(f) = false) 
    [2L] |> Seq.append(intermediate)
        
    

            
