// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

// Define a function to construct a message to print
let initialState = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.head
    |> (fun line -> Seq.toList (line.Split(',')))
    |> List.map (fun s -> Int32.Parse(s))

let afterDays days state =
    [ 1 .. days ]
    |> List.fold 
        (fun (s, nf) _ -> 
            let ns = s |> List.map (fun x -> x - 1) |> List.map (fun x -> if x < 0 then 6 else x)
            let zeros = ns |> List.filter (fun x -> 0 = x) |> List.length
            (List.append ns [ for i in 1 .. nf -> 8 ], zeros))
        (state, 0)
    |> fst

[<EntryPoint>]
let main argv =

    initialState
    |> afterDays 80
    |> List.length
    |> printfn "part one: %A"

    0 // return an integer exit code