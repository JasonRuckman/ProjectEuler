module Helpers

open System
open System.Collections.Generic

let max (a : int64, b : int64) : int64 = 
    if a > b then a
    else b
            

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
    
