module LetterHelper

open System
open System.Text

let generateOnes (a : int) = 
    match a with
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | _ -> raise (Exception())

let generateTeens (a : int) = 
    match a with 
        | 10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | 13 -> "thirteen"
        | 14 -> "fourteen"
        | 15 -> "fifteen"
        | 16 -> "sixteen"
        | 17 -> "seventeen"
        | 18 -> "eighteen"
        | 19 -> "nineteen"
        | _  -> raise (Exception())
        
let generateTens (a : int) = 
    match a with 
        | 20 -> "twenty"
        | 30 -> "thirty"
        | 40 -> "forty" 
        | 50 -> "fifty"
        | 60 -> "sixty"
        | 70 -> "seventy"
        | 80 -> "eighty"
        | 90 -> "ninety"
        | _ -> raise (Exception())

let decompose (a : int, radix : int) = 
    let num = a % (10 * radix)
    num     

let getLengthFromNumber (a : int) = 
    let mutable value = a 
    let builder = new StringBuilder()
    
    while value > 0 do
        ignore(builder.Append(match value with 
                                | v when value > 100 -> generateOnes(decompose(v, 3)) + " hundred"
                                | v when value > 19 -> generateTens(v)
                                | v when value >= 10 -> generateTeens(v)
                                | _ -> generateOnes(value)))

        value <- value / 10

    builder.ToString().Length
                
            


