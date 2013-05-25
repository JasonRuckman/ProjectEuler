module Problems11Through20

open System
open System.IO
open System.Linq
open System.Collections.Generic

open System.Numerics
open Structures

let computeProduct (i : int, j : int, lines : int64 array array) = seq { for offset = 0 to 3 do yield offset } |> Seq.map(fun f -> lines.[i - f].[j + f]) |> Seq.reduce(fun acc item -> acc * item)   
let generateAllLeftToRightDiagonalItems (lines : int64 array array) = seq { for i = 0 to 16 do for j = 0 to 16 do yield computeProduct(i, j, lines) }
let generateAllRightToLeftDiagonalItems (lines : int64 array array) = seq { for i = 19 downto 3 do for j = 0 to 16 do yield computeProduct(i, j, lines) }


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
    let gen f = 
        let mutable v = 0
        for i in f .. -1 .. 0 do 
            v <- v + i
        Some(v, f + 1)

    Seq.unfold(fun f -> gen f) 1 

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

let problemFifteen = 
    let f = BigInteger.Divide(Helpers.factorial(20L + 20L) , (Helpers.factorial(20L) * Helpers.factorial(20L)))
    f

let problemSixteen unit =
     BigInteger.Pow(bigint 2, 1000).ToString() 
        |> Seq.map(fun f -> BigInteger.Parse(f.ToString())) 
        |> Seq.sum

let problemSeventeen int = 
    seq { for i in 1 .. 1000 -> i } 
    |> Seq.map(fun f -> LetterHelper.getLengthFromNumber(f))
    |> Seq.sum


let problemEighteen() = 
    let ps = [Environment.NewLine;] |> Seq.toArray
    let i = "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
    let items = i.Split(ps, StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun f -> f.Split() |> Seq.map(fun v -> v |> Int32.Parse) |> Seq.toArray) |> Seq.toArray
    let graph = new Graph(items)
    let result = graph.Max()
    ()

let problemNineteen unit = 
    let mutable seed = new DateTime(1901, 1, 1)
    let ed = new DateTime(2000, 12, 31)

    let mutable numSundays = 0

    while seed.DayOfWeek <> DayOfWeek.Sunday do
        seed <- seed.AddDays(1.0)

    while seed < ed do 
        seed <- seed.AddDays(7.0)
        if seed.DayOfWeek = DayOfWeek.Sunday && seed.Day = 1 then numSundays <- numSundays + 1

    numSundays

let problemTwenty unit = 
    let factorial = 
        Seq.init 100 (fun f -> new BigInteger(100 - f)) 
        |> Seq.reduce(fun acc item -> acc * item)

    factorial.ToString() |> Seq.map(fun f -> BigInteger.Parse(f.ToString())) |> Seq.sum