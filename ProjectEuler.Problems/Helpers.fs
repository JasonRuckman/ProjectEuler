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

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let rotateOne a radix = 
    let mutable num = a

    let rem = num % 10
    num <- num / 10
    (double(num) + (double(rem) * Math.Pow(10.0, double(radix)))) |> Convert.ToInt32


let rotate a = 
        if a < 10 then seq { yield a }
        else   
            let radix = int(Math.Log10(a |> Convert.ToDouble))
            let mutable first = rotateOne a radix
            let r = ref first

            seq { 
                    while r.Value <> a do
                        yield r.Value
                        r.Value <- rotateOne r.Value radix 
                }  
            

              
            
            

let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }


let sqrt_int(x:int) = x |> float |> sqrt |> int
let sqrt_int64(x:int64) = x |> float |> sqrt |> int64

let factor (a : int) = 
     seq {
            for i = 1 to a / 2 do
                if(a % i = 0) then
                   yield i
         }
       
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

let primeSieve a = 
    let knownComposites = new HashSet<int64>()

    for i in 3L .. 2L .. a do
       for j in 2L * i .. i .. a do
        if knownComposites.Contains(j) = false then ignore(knownComposites.Add(j))

    let intermediate = seq { for i in 3L .. 2L .. a -> i } |> Seq.filter(fun f -> knownComposites.Contains(f) = false) 
    [2L] |> Seq.append(intermediate)

let primeSieveInt a = 
    let knownComposites = new HashSet<int>()

    for i in 3 .. 2 .. a do
       for j in 2 * i .. i .. a do
        if knownComposites.Contains(j) = false then ignore(knownComposites.Add(j))

    let intermediate = seq { for i in 3 .. 2 .. a -> i } |> Seq.filter(fun f -> knownComposites.Contains(f) = false) 
    [2] |> Seq.append(intermediate)
        
    

            
