// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let getInputList = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.map (fun line -> line.Split(" "))

let (|Forward|_|) = function
    | [|e1;e2;|] when e1 = "forward" -> Some(Int32.Parse(e2))
    | _ -> None

let (|Down|_|) = function
    | [|e1;e2;|] when e1 = "down" -> Some(Int32.Parse(e2))
    | _ -> None

let (|Up|_|) = function
    | [|e1;e2;|] when e1 = "up" -> Some(Int32.Parse(e2))
    | _ -> None

let calc iv mf rf =
    getInputList
    |> Seq.fold mf iv
    |> rf

[<EntryPoint>]
let main argv =

    calc (0, 0)
        (fun acc direction -> 
            let (h, d) = acc
            match direction with
            | Forward x -> (h + x, d)
            | Down x -> (h, d + x)
            | Up x -> (h, d - x)
            | _ -> acc
        )
        (fun (h, d) -> h * d)
    |> printfn "first: %d"

    calc (0, 0, 0)
        (fun acc direction -> 
            let (h, d, a) = acc
            match direction with
            | Forward x -> (h + x, d + x * a, a)
            | Down x -> (h, d, a + x)
            | Up x -> (h, d, a - x)
            | _ -> acc
        ) 
        (fun (h, d, _) -> h * d)
    |> printfn "second: %d"

    0 // return an integer exit code