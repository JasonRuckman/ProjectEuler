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
            
let isPalindrome a = 
    let mutable reversedNum = 0
    let mutable num = a

    while num > 0 do
        reversedNum <- (reversedNum * 10) + (num % 10)
        num <- num / 10

    reversedNum = a

let toBitArray a = 
    let buf = Array.create(32) false

    for i = 0 to buf.Length - 1 do
        buf.[buf.Length - 1 - i] <- a &&& (1 <<< i) <> 0
    
    buf

let reverse (a : 'a array) = 
    let mutable start = 0
    let mutable e = a.Length - 1
    let buf = Array.zeroCreate(a.Length)

    while start < e do
        buf.[start] <- a.[e]
        buf.[e] <- a.[start]
        start <- start + 1
        e <- e - 1

    buf

let digits a = 
    let mutable num = a
    let mutable rem = 0
    let mutable d = List.empty<int>

    while num > 0 do
        rem <- num % 10
        num <- num / 10
        d <- rem :: d

    d

let intLog10 (a : int) = int(Math.Log10(float(a)))

let breakup a = 
    let mutable l = List.empty
    let mutable num = a
    while num > 0L do 
        let rem = num % 10L
        num <- num / 10L
        l <- rem :: l
    l

let foldLongList a = 
    let mutable num = 0L
    let mutable radix = 1L

    for i in (a |> List.rev) do
        num <- num + (i * radix)
        radix <- radix * 10L
        
    num
      
let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }


let sqrt_int(x:int) = x |> float |> sqrt |> int
let sqrt_int64(x:int64) = x |> float |> sqrt |> int64

let factor (a : int) = seq { for i = 1 to a / 2 do if(a % i = 0) then yield i }
       
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
        
    

            
