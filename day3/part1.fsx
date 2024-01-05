open System
open System.IO
open System.Globalization

let input = File.ReadAllLines("./day3/input.txt")
let testInput = File.ReadAllLines("./day3/test.txt")

// schematic holds numbers and symbols
// numbers adjacent to symbol even diagnally are part numbers; (.) doesnt count
type coordinate = { x: int; y: int }
let areaRightOfStart (x: int) (y: int) length =
    seq {
        for i in 0 .. (length) do
            yield { y = y; x = x + i }
    }

type element =
    { coordinate: coordinate
      length: int 
      symbol: bool
      }



    member this.area =
        seq {
            yield
                { y = this.coordinate.y - 1
                  x = this.coordinate.x - 1 }

            yield! areaRightOfStart this.coordinate.x (this.coordinate.y - 1) this.length

            yield
                { y = this.coordinate.y
                  x = this.coordinate.x - 1 }

            yield! areaRightOfStart this.coordinate.x this.coordinate.y this.length

            yield
                { y = this.coordinate.y + 1
                  x = this.coordinate.x - 1 }

            yield! areaRightOfStart this.coordinate.x (this.coordinate.y + 1) this.length
        }

//y -1 +1
// x -1 +length

type symbol = element

let createSymbol x y =
    { coordinate = { x = x; y = y }
      length = 1 
      symbol = true}

let createElement c l = { coordinate = c; length = l; symbol = false }


type Schematic =
    { partNumbers: element list
      symbols: symbol list }

// let getNumber (idx: int) (line: string) =
//     let nextDot = line[idx..].IndexOf(".")
//     if nextDot = -1 then 
//         line.Substring(idx) |> Int32.Parse
//     else
//         line.Substring(idx, nextDot) |> Int32.Parse

let getNumber (idx: int) (line: string) =
    let endOfNumber = line.Substring(idx).ToCharArray()|> Array.tryFindIndex (fun c ->  Char.IsDigit c |> not  ) 
    match endOfNumber with 
    | None ->
        //printfn "no end idx %i" idx
        line.Substring(idx) |> Int32.Parse
    | Some endOfNumber ->
        //printfn "idx %i" idx
        //printfn "end of number %A" endOfNumber
        //printfn "length %i" line.Length
        //printfn "line %A" line
        line.Substring(idx, endOfNumber) |> Int32.Parse



let parseLine yCord (line:string)   =
    let rec loop (l:string) idx acc  =  
        match l.Length > idx with 
        | true -> 
                if l[idx] = '.' || Char.IsWhiteSpace l.[idx] then loop l (idx + 1) acc
                elif Char.IsDigit(l[idx]) 
                  then 
                    let num = getNumber idx line 
                    let length = Math.Log10(float num) + 1. |> int
                    let ele = createElement { x = idx; y = yCord } length 
                    loop l (idx + length-1) (ele::acc)
                else 
                    let ele = createSymbol idx yCord
                    loop l (idx + 1) (ele::acc)
        | false -> acc    
    
    loop line 0 [] 


//function to find all numbers in a line and return if the line is ended

//take the function above and map the results to schematic 

// go over the schematic and get area of each to figure out if we
// should sum the number

//ignore all the logic in the match but use this once you want to 
//go over the schematic 
let parseLines (input: string array) =
    let rec loop (lines: string array) idx (sum:element list) =
        match lines.Length > idx with
        | true ->
            match idx with
            | 0 -> //top row
             let pl = parseLine 0 lines[idx]
             loop lines (idx + 1) pl@sum
            | x when x = input.Length - 1 -> //last row
             let pl = parseLine x lines[idx]
             loop lines (idx + 1) pl@sum
            | x when x = input.Length ->
             sum //end
            | x ->  //middle
             let pl = parseLine x lines[idx]
             loop lines (idx + 1) pl@sum
        | false -> sum


    loop input 0 []


// let foo = createElement { x = 2; y = 2 } 2
// foo.area

// let bar = createElement { x = 2; y = 2 } 2
//// bar.area |> Seq.toList |> printfn "%A"
let getElementFromArray (arr:string array) (e:element) = 
    let x =arr[e.coordinate.y][e.coordinate.x .. e.coordinate.x + e.length - 1]
        |> Int32.Parse
    // printfn "%A" x
    // printfn "%A" e.coordinate
    x
let res = testInput |> parseLines
res.Length
let numbers = res |> Seq.filter (fun e -> e.symbol = false)
let sybolCords = res |> Seq.filter (fun e -> e.symbol = true) |> Seq.map (fun e -> e.area) |> Seq.concat 
let pa = getElementFromArray testInput 
let partNumbers = numbers |> Seq.filter (fun e ->  Seq.contains e.coordinate sybolCords || Seq.contains {y=e.coordinate.y;x=e.coordinate.x + e.length} sybolCords) |> Seq.toList
Seq.fold (fun acc e -> acc + (pa e)) 0 partNumbers

partNumbers |> Seq.map (fun x-> pa x)  
numbers |> Seq.length
numbers |> Seq.map (fun f-> pa f) |> Seq.length
partNumbers |> Seq.length
//sybolCords |> Seq.contains { x = 7; y = 5 } |> printfn "%A"
//sybolCords|> Seq.iter (fun e -> printfn "%A" <| testInput[e.y][e.x .. e.x + 1 - 1])


let res' = input |> parseLines
let numbers' = res' |> Seq.filter (fun e -> e.symbol = false)
let sybolCords' = res' |> Seq.filter (fun e -> e.symbol = true) |> Seq.map (fun e -> e.area) |> Seq.concat 
let pa' = getElementFromArray input 
let partNumbers' = numbers' |> Seq.filter (fun e ->  Seq.contains e.coordinate sybolCords' || Seq.contains {y=e.coordinate.y;x=e.coordinate.x + e.length} sybolCords') |> Seq.toList
partNumbers'|> Seq.map (fun x-> pa' x) |> Seq.sum
Seq.fold (fun acc e -> acc + (pa' e)) 0 partNumbers'
partNumbers'|> Seq.length
let intPartNumbers = partNumbers' |> Seq.filter(fun f-> f.coordinate.y = 0 ) |> Seq.map (fun x-> pa' x)|>  Seq.iter (fun x -> printfn "%A" x)
partNumbers'|> Seq.map (fun x-> pa' x) |> Seq.filter (fun x -> x<0)
