open System
open System.IO

let input = File.ReadAllLines("./day2/input.txt")

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
type bag = { red: cube; blue: cube; green: cube }


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

let bagCount =
    { red = Red 12
      blue = Blue 14
      green = Green 13 }

let possibleGames  (bag: bag) (games: game array)=
    games
    |> Array.fold
        (fun s g ->
            let possible =
                g.cubes
                |> Array.fold
                    (fun s c ->
                        let foo =
                          c
                              |> Array.map (fun r ->
                                  match r with
                                  | Red r when Red r > bag.red -> false
                                  | Blue b when Blue b > bag.blue -> false
                                  | Green g when Green g > bag.green -> false
                                  | _ -> true

                              )|> Array.fold (&&) true
                        printfn "this round is %b : %A in Game Id %i" foo c g.id
                        foo && s
                        )
                    true
            // printfn "this game is %b : %A" possible g
            if possible then
               printfn "Adding: %i" g.id
               s + g.id
            else s)
        0
Games |> possibleGames bagCount 

