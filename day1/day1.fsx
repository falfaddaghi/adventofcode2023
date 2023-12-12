module day1

open System
open System.IO

let maptoNumber  = function 
| "one" -> Some "1"
| "two" -> Some "2"
| "three" -> Some "3"
| "four" -> Some "4"
| "five" -> Some "5"
| "six" -> Some "6"
| "seven" -> Some "7"
| "eight" -> Some "8"
| "nine" -> Some "9"
| _ -> None

let getSubStringLength (char:char) = 
  match char with 
  |'o'-> [3]
  |'t' -> [3;5]
  |'f' -> [4]
  |'s' -> [3;5]
  |'e' ->[5]
  |'n' -> [4]
  |_ -> []
  
let coundBeNum c =
  match c with
  |'o'->true 
  |'t' ->true
  |'f' ->true
  |'s' ->true
  |'e' ->true
  |'n' ->true
  |_ -> false 
  //you can just *10 in base 10 to shift numbers to the left by 1 digit (big dummy is me)
let combineNumb x y=
  int32(x.ToString() + y.ToString())

let day1_part1 (input:string[]) =
  let rec loop (input:string[]) (acc:int32) = 
    match input with
    |[||] -> acc
    |a ->  
      let x = a.[0]
      let xs = a.[1..]
      let numChar = x.ToCharArray() |> Array.filter Char.IsDigit  
      let num = if numChar.Length >1 then combineNumb numChar.[0] numChar.[numChar.Length-1] else combineNumb numChar.[0] numChar.[0]
      loop xs (acc+num)
  loop input 0
let day1_part1_fold (input: string []):int = 
  input |> Array.fold (fun x y -> 
  let numChar = y.ToCharArray() |> Array.filter Char.IsDigit 
  let num = if numChar.Length >1 then Int32.Parse(numChar[0].ToString()+numChar[numChar.Length-1].ToString()) else Int32.Parse(numChar[0].ToString() + numChar[0].ToString())
  printfn "%i" num
  num + x ) 0

  //too low you noob
let foo (str: string) : string list =
    printfn "working on : %s " str
    let rec loop (s: string) idx (nums: string list) =
        if idx = s.Length then
            printfn "extracted numbers %A" nums
            nums
        else 
        let c = s.[idx]
        printfn "current char %c" c
        if Char.IsDigit c then 
            loop s (idx + 1) (nums@[c.ToString()])
        else 
        match coundBeNum c with
        | true ->
            let subStringLength = getSubStringLength c
            let bar =
              subStringLength
              |> List.filter (fun x -> x <= s.Length - idx)
              |> List.map (fun x -> 
              let baz = str.Substring(idx, x) 
              printfn "sub string %s" baz
              baz 
              |> maptoNumber ,x )
              |> List.filter (fun f-> fst f |>  Option.isSome)
            match bar with 
            | [] -> loop str (idx + 1) (nums)
            |h::hss ->
              let baz = Option.get (fst h)
              loop str (idx + (snd h ))  (nums@[baz])
        | false -> loop str (idx + 1) (nums)
    loop str 0 []  


let day1_part2 (input:string[]) =
  input
  |> Array.fold (fun s e-> 
    let res = foo e
    let sn = res.Head + List.last res
    printfn "%s" sn
    s + int32(sn)
  ) 0
let input = File.ReadAllLines("input.txt") 
input |> day1_part1
input |> day1_part2