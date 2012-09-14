module Problems21Through30

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics


let problemTwentyOne unit =
    let something = seq { for i in 1 .. 10000 -> i } 
                        |> Seq.map(fun f -> 
                            let sumOfFactors = Helpers.factor(f) |> Seq.sum
                            let otherSumOfFactors = Helpers.factor(sumOfFactors) |> Seq.sum 

                            (sumOfFactors, f, otherSumOfFactors))
                        |> Seq.filter(fun (sumOfFactors, f, otherSumOfFactors) -> otherSumOfFactors = f)
                        |> Seq.map(fun (sumOfFactors, f, otherSumOfFactors) -> [sumOfFactors ; f ])
                        |> Seq.toList
                        |> Seq.concat
                        |> Seq.distinct
                        |> Seq.sum

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