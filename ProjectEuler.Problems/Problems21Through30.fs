module Problems21Through30

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Linq
open System.Numerics


let problemTwentyOne() =
    let candidateCache = new Dictionary<int, int>()
    let sumFactors f = Helpers.factor(f) |> Seq.sum
    let candidates = [2 .. 10000] 
                        |> Seq.choose(fun f -> let sum = sumFactors f
                                               ignore(candidateCache.Add(f, sum))
                                               if sum <> 1 then Some(f, sum) else None)  |> Seq.toArray

    let amicables = new HashSet<int>()

    let isAmicableWithPartners a = 
        candidates 
            |> Seq.filter(fun f -> fst f <> fst a) 
            |> Seq.iter(fun x -> 
                            let q = candidateCache.[fst a]
                            let r = candidateCache.[fst x] 
                            if (fst a) = r && (fst x) = q then 
                                if amicables.Contains(fst a) <> true then ignore(amicables.Add(fst a))
                                if amicables.Contains(fst x) <> true then ignore(amicables.Add(fst x))    
                            )
                            
    candidates |> Seq.iter(fun f -> isAmicableWithPartners(f))
    let sum = amicables |> Seq.sum
    ()

let problemTwentyTwo = 
    
    let alphabet = ['a' ; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z' ]

    let dictionary = new Dictionary<char, int>();
    alphabet |> Seq.iteri(fun index f -> dictionary.Add(f, index))

    let input = File.ReadAllText("names.txt").ToLower().Split(',')
 
    Seq.sort(input)
      |> Seq.map(fun f -> f.Replace("\"", String.Empty))
      |> Seq.mapi(fun index f -> 
        Convert.ToInt64((f.ToCharArray() |> Seq.map(fun v -> dictionary.[v] + 1) |> Seq.sum)) * Convert.ToInt64((index + 1)))
      |> Seq.sum  


let problemTwentyFour unit = 
    let result = 
        ("0123456789" |> Seq.toList |> Helpers.permute |> Seq.sort |> Seq.nth(999999) 
        |> Seq.fold(fun acc item -> String.Format("{0}{1}", acc, item)) String.Empty) 
        |> Int32.Parse
    ()

let problemTwentyFive unit = 
    let fib = Seq.unfold(fun (current, next) -> Some(current, (next, current + next))) (bigint 0, bigint 1)

    let result = fib
                 |> Seq.mapi(fun index f -> (index, f.ToString()))
                 |> Seq.filter(fun (index, f) -> f.Length = 1000)
                 |> Seq.head 
                 |> fst
    ()

let problemTwentyThree() =    
    let abundants = [12..28123] |> Seq.filter(fun f -> (Helpers.factor(f) |> Seq.sum) > f ) |> Seq.toArray
    let sumCache =  seq { for i in abundants do for j in abundants do yield i + j } |> Set.ofSeq           
    let result = [1..28123] |> Seq.filter(fun f -> sumCache.Contains(f) |> not) |> Seq.sum

    ()

let generateCoPrimePairs (upTo : int) : seq<int * int> = 
        let range = [1..upTo]
        seq { 
            for i in range do
                for j in range do
                    let iFactors = Helpers.factor(i)
                    let jFactors = Helpers.factor(j)
                    if iFactors |> Seq.forall(fun f -> j / f <> 0) && jFactors |> Seq.forall(fun f -> i / f <> 0) then yield (i,j) 
            }

let rec gcd x y =
    if y = 0.0 then x
    else gcd y (x % y)

let findMultiplicativeOrder a n = 
    if gcd(a) n = 1.0 then 0.0
    else 
        let mutable order = 1.0
        let mutable mod_exp = a
        while mod_exp <> 1.0 do
            order <- order + 1.0
            mod_exp <- (mod_exp * a) % n
        order

let problemTwentySix() = 
    let d = [for i in 1.0..1000.0 -> (i, 1.0 / i)] |> Seq.map(fun (i, quot) -> findMultiplicativeOrder(10.0) i) |> Seq.toList
    ()

let problemTwentyNine() = 
    let totals = [bigint 2..bigint 100] |> Seq.collect(fun f -> [2..100] |> Seq.map(fun x -> BigInteger.Pow(f, x))) |> Seq.distinct |> Seq.length
    ()

let problemThirty() = 
    let pow c = Math.Pow(Double.Parse(c.ToString()), 5.0)
    let result = [for i in 2.0 .. 10.0 ** 6.0 -> i] 
                    |> Seq.filter(fun i -> i = (i.ToString() |> Seq.map(fun f -> pow(f)) |> Seq.sum))
                    |> Seq.sum
    ()
 
        

