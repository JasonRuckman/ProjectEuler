module Problems21Through30

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics


let problemTwentyOne() =
    let set = new HashSet<int>()
    let candidateCache = new Dictionary<int, int>()

    let candidates = [2 .. 10000] 
                        |> Seq.map(fun f -> 
                                    let sum = Helpers.factor(f) |> Seq.sum
                                    ignore(candidateCache.Add(f, sum))
                                    (f, sum)
                                  ) 
                        |> Seq.filter(fun f -> (snd f) <> 1) |> Seq.toArray

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

    let input = File.ReadAllText("C:\\Users\\Jason Ruckman.ALTERIAN\\Documents\\GitHub\\ProjectEuler\\Files\\names.txt").ToLower().Split(',')
 
    Seq.sort(input)
      |> Seq.map(fun f -> f.Replace("\"", String.Empty))
      |> Seq.mapi(fun index f -> 
        Convert.ToInt64((f.ToCharArray() |> Seq.map(fun v -> dictionary.[v] + 1) |> Seq.sum)) * Convert.ToInt64((index + 1)))
      |> Seq.sum  


let problemTwentyFour unit = 
    let result = "0123456789" |> Seq.toList |> Helpers.permute |> Seq.sort |> Seq.nth(999999) |> Seq.fold(fun acc item -> String.Format("{0}{1}", acc, item)) String.Empty
    let integerResult = Int32.Parse(result)
    ()

let problemTwentyFive unit = 
    let result = Seq.unfold(fun (current, next) -> Some(current, (next, current + next))) (new BigInteger(0), new BigInteger(1))
                 |> Seq.mapi(fun index f -> (index, f))
                 |> Seq.filter(fun (index, f) -> f.ToString().Length = 1000)
                 |> Seq.head
    ()

let problemTwentyThree() = 
    let isAbundant a =
        (Helpers.factor(a) |> Seq.sum) > a 
       
    let abundants = [12..28123] |> Seq.filter(fun f -> isAbundant(f)) |> Seq.toArray
    
    let sumsOfAbundants =  seq { for i in abundants do
                                    for j in abundants do
                                        yield i + j 
                               }

    let sumCache = new HashSet<_>(sumsOfAbundants)
                       
    let result = [1..28123] |> Seq.filter(fun f -> sumCache.Contains(f) |> not) |> Seq.sum

    ()