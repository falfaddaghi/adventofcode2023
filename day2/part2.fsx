
open System
open System.IO

let input = File.ReadAllLines("./day2/input.txt")
let testInput = File.ReadAllLines("./day2/test.txt")

type cube =
    | Red of int
    | Blue of int
    | Green of int

    static member FromString (srt: string) (number: int) : cube =
        match srt with
        | "red" -> Red number
        | "blue" -> Blue number
        | "green" -> Green number
        | _ -> failwith "Unknown color"
      


type round = cube array
type game = { id: int; cubes: round array }
type bag = 
  { red: cube; blue: cube; green: cube }
  member this.Power =
    match this.red, this.blue, this.green with
    | Red r, Blue b, Green g -> r * b * g
    | _, _, _ -> 0// Handle any cases not explicitly covered

let parseGame (s: string) =
    let part1 = s.Substring(0, s.IndexOf(":"))
    let num = part1.Substring(part1.IndexOf(" ")) |> Int32.Parse
    printfn "%i" num
    // part 2
    let part2Start = part1.Length + 1
    let games = s.Substring(part2Start, s.Length - part2Start).Split([| ';' |]) //- part2Start
    printfn "%A" games
    // part 3 (parse games)
    let foo =
        games
        |> Array.map (fun g -> g.Split([| ' '; ',' |], options = StringSplitOptions.RemoveEmptyEntries))

    let cubes =
        foo
        |> Array.map (fun g ->
            g
            |> Array.mapi (fun i s ->
                if i % 2 = 0 then
                    let number = Int32.Parse(s)
                    let color = g.[i + 1]
                    let cube = cube.FromString color number
                    Some cube
                else
                    None)
            |> Array.choose id)

    let ga = { id = num; cubes = cubes }
    ga

let Games = input |> Array.map parseGame

let PowerOfAllGames (games: game array)=
    games
    |> Array.fold
        (fun s g ->
                let l =
                   g.cubes
                    |> Array.fold
                    (fun s c ->
                     let leastPossible = 
                        c
                        |> Array.fold (fun s n ->
                                          match n with 
                                          | Red r -> if Red r>s.red then {s with red= Red r} else s
                                          | Blue r-> if Blue r>s.blue then {s with blue=Blue r} else s 
                                          | Green r-> if Green r>s.green then {s with green=Green r} else s 
                                        ) s 
                     leastPossible
                    ){red = Red 0; blue = Blue 0; green = Green 0}
                l.Power + s
        )
        0


#time 
Games |> PowerOfAllGames  
let test = testInput |> Array.map parseGame
test |> PowerOfAllGames 
#time