// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

// Define a function to construct a message to print
let positions = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadAllText
    |> (fun s -> s.Split(','))
    |> Seq.toList
    |> List.map (fun s -> Int32.Parse(s))

let calcFuel1 fromPos toPos  = if fromPos < toPos then toPos - fromPos else fromPos - toPos

let calcFuel2 fromPos toPos  = 
    [ 1 .. (calcFuel1 fromPos toPos) ]
    |> List.fold (fun acc n -> acc + n) 0

let calcTotalFuel calcFuel d ps =
    ps
    |> List.map (fun p -> calcFuel p d)
    |> List.sum

[<EntryPoint>]
let main argv =

    positions
    |> (fun ps -> (List.min ps, List.max ps))
    |> (fun (min, max) -> [ min .. max ])
    |> List.map (fun p -> calcTotalFuel calcFuel1 p positions)
    |> List.min
    |> printfn "part one: %A"

    positions
    |> (fun ps -> (List.min ps, List.max ps))
    |> (fun (min, max) -> [ min .. max ])
    |> List.map (fun p -> calcTotalFuel calcFuel2 p positions)
    |> List.min
    |> printfn "part two: %A"

    0 // return an integer exit code