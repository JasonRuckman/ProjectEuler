module Problems11Through20

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics

let problemThirteen (path : String) = 
    let sum = File.ReadAllLines(path) |> Seq.map(fun f -> BigInteger.Parse(f)) |> Seq.sum 
    sum.ToString().Substring(0, 10)