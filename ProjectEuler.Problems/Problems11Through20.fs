module Problems11Through20

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics

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