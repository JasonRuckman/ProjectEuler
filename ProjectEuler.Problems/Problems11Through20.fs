﻿module Problems11Through20

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics

let generateAllLeftToRightDiagonalItems (lines : int64 array array) = 
    //for each line, attempt to create a sequence that involves

    let computeProduct (i : int, j : int) = 
        seq { for offset = 0 to 3 do yield offset } 
        |> Seq.map(fun f -> 
            lines.[i + f].[j + f]) 
        |> Seq.reduce(fun acc item -> acc * item)

    seq { for i = 0 to 16 do for j = 0 to 16 do yield computeProduct(i, j) }
 
let generateAllRightToLeftDiagonalItems (lines : int64 array array) = 
    //for each line, attempt to create a sequence that involves

    let computeProduct (i : int, j : int) = 
        seq { for offset = 0 to 3 do yield offset } 
        |> Seq.map(fun f -> 
            lines.[i - f].[j + f]) 
        |> Seq.reduce(fun acc item -> acc * item)

    seq { for i = 19 downto 3 do for j = 0 to 16 do yield computeProduct(i, j) }


let problemEleven (path : String) = 
    let lines = File.ReadAllLines(path) 
                    |> Seq.map(fun f -> f.Split(' '))
                    |> Seq.map(fun f -> f |> Seq.map(fun v -> Int64.Parse(v)) |> Seq.toArray)
                    |> Seq.toArray

    let maxLeftToRightProduct = lines 
                                |> Seq.map(fun f -> 
                                               f |> Seq.windowed(4) |> Seq.map(fun v -> v |> Seq.reduce(fun acc item -> acc * item)) |> Seq.max)

    let maxRightToLeftProduct = lines
                                |> Seq.map(fun f -> f.Reverse() |> Seq.windowed(4) |> Seq.map(fun v -> v |> Seq.reduce(fun acc item -> acc * item)) |> Seq.max)

    let mutable maxUpToDownProduct = Seq.empty<int64>
    let mutable maxDownToUpProduct = Seq.empty<int64>

    for i = 0 to 19 do 
        let arr = lines 
                    |> Seq.map(fun f -> f.[i]) 
                    |> Seq.windowed(4) 
                    |> Seq.map(fun v -> v |> Seq.reduce(fun acc item -> acc * item))

        maxUpToDownProduct <- maxUpToDownProduct |> Seq.append(arr)

    for i = 0 to 19 do 
        let arr = lines |> Seq.map(fun f -> f.[i]) 
        let reverseArr = arr.ToArray().Reverse() 
                            |> Seq.windowed(4) 
                            |> Seq.map(fun v -> v 
                                                |> Seq.reduce(fun acc item -> 
                                                                        acc * item))

        maxDownToUpProduct <- maxUpToDownProduct |> Seq.append(reverseArr)    

    let leftToRightDiagonal = generateAllLeftToRightDiagonalItems(lines)
    let rightToLeftDiagonal = generateAllRightToLeftDiagonalItems(lines)

    maxLeftToRightProduct 
    |> Seq.append(maxRightToLeftProduct) 
    |> Seq.append(maxUpToDownProduct) 
    |> Seq.append(maxDownToUpProduct)
    |> Seq.append(leftToRightDiagonal)
    |> Seq.append(rightToLeftDiagonal) 
    |> Seq.max

    
let triangleGenerator unit = 
        Seq.unfold(fun f -> 
        let mutable v = 0
        for i in f .. -1 .. 0 do 
            v <- v + i
        Some(v, f + 1)) 1 

let factors number = seq {
    for divisor in 1 .. (float >> sqrt >> int) number do
    if number % divisor = 0 then
        yield divisor
        yield number / divisor
}

let problemTwelve unit = 
    triangleGenerator()
    |> Seq.filter(fun f -> factors(f) |> Seq.distinct |> Seq.length > 500)
    |> Seq.head

let problemThirteen (path : String) = 
    let sum = File.ReadAllLines(path) |> Seq.map(fun f -> BigInteger.Parse(f)) |> Seq.sum 
    sum.ToString().Substring(0, 10)

let problemFourteen (a : int64) = 
        let chainDictionary = new Dictionary<int64, int64>()

        let generateChain (b : int64) = 
            let mutable m = b
            let mutable count = 0L

            while m >= 1L do
                if m = 1L then
                    count <- count + 1L
                    m <- 0L
                elif chainDictionary.ContainsKey(m) then
                    count <- count + chainDictionary.[m]
                    m <- 0L
                elif m % 2L = 0L then 
                    count <- count + 1L
                    m <- m / 2L       
                else 
                    count <- count + 1L
                    m <- 3L * m + 1L

            chainDictionary.Add(b, count)
            count
    
        seq { for i = 1L to a do yield i } |> Seq.map(fun f -> (f, generateChain(f))) |> Seq.maxBy(fun (k, v) -> v)

let problemSixteen unit =
     BigInteger.Pow(BigInteger(2), 1000).ToString() 
        |> Seq.map(fun f -> BigInteger.Parse(f.ToString())) 
        |> Seq.sum

let problemTwenty unit = 
    let factorial = 
        Seq.init 100 (fun f -> new BigInteger(100 - f)) 
        |> Seq.reduce(fun acc item -> acc * item)

    factorial.ToString() |> Seq.map(fun f -> BigInteger.Parse(f.ToString())) |> Seq.sum