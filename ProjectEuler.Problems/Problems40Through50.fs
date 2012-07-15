module Problems40Through50

open System.Numerics

let problemFortyEight unit = 
    let num = Seq.init 1000 (fun f -> BigInteger.Pow(BigInteger.Parse((f + 1).ToString()), f + 1)) |> Seq.fold(fun acc item -> acc + item) 0I 
    let output = num.ToString()
    output.Substring(output.Length - 10, 10)
    
