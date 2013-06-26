module Problems31Through40

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Linq
open System.Numerics

let problemThirtyFour() = 
     let map = new Dictionary<int, int>()
     
     for i in 0 .. 9 do
        let mutable product = 1
        for j in 1 .. i do
            product <- product * j
        map.Add(i, product)  
     //for really large factorials, you probably want to cache the digit chain, but for the smaller ones it doesn't seem to be worth it
     let sum = seq { for i in 10 .. 2540160 do if ((i |> Helpers.digits |> Seq.map(fun f -> map.[f])) |> Seq.sum) = i then yield i } |> Seq.sum
     ()

let problemThirtyFive() = 
    let primeCache = new HashSet<int>();
    let b = Helpers.primeSieveInt(1000000) |> Seq.toList
    let cache = new HashSet<int>(b)   
    let toAdd = new List<int>()

    let add a = primeCache.Add(a) |> ignore
    let addToListAndReturn a = 
        toAdd.Add(a)
        a

    for i in b do
        toAdd.Clear()
        if (Helpers.rotate i) |> Seq.map(addToListAndReturn) |> Seq.forall(cache.Contains) then [i] |> Seq.append toAdd |> Seq.iter(add)

    ()

let problemThirtySix() =  
    let palindromes = seq { for i in 1 .. 2 .. 999999 do if Helpers.isPalindrome i then yield i }
    let isBitArrayPalindrome a = 
        let noZeros j = j |> Seq.skipWhile(fun f -> f <> true) |> Seq.toArray
        let rev = a |> Helpers.reverse |> noZeros
        let actual = a |> noZeros

        let result = (rev) |> Seq.zip(actual) |> Seq.forall(fun f -> (fst f) = (snd f))

        result

    let sum = palindromes |> Seq.choose(fun f -> if (Helpers.toBitArray(f) |> isBitArrayPalindrome) then Some(f) else None) |> Seq.sum
    1
            