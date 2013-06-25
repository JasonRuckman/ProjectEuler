module Problems31Through40

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Linq
open System.Numerics

let problemThirtyFive() = 
    let primeCache = new HashSet<int>();
    let b = Helpers.primeSieveInt(1000000) |> Seq.toList
    let cache = new HashSet<int>(b)
    let add a = primeCache.Add(a) |> ignore
    let toAdd = new List<int>()

    let addToListAndReturn a = 
        toAdd.Add(a)
        a

    for i in b do
        toAdd.Clear()
        if (Helpers.rotate i) |> Seq.map(addToListAndReturn) |> Seq.forall(cache.Contains) then 
            toAdd.Add(i)
            toAdd |> Seq.iter(add)
        
    ()
            