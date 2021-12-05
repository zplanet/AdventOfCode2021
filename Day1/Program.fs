// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let getInputList = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.map (fun line -> Int32.Parse(line))
    |> Seq.toList

let first = 
    let rec iter r ls = 
        match ls with
        | e1::e2::t -> if e1 < e2 then iter (r+1) (e2::t) else iter r (e2::t)
        | _ -> r

    getInputList
    |> iter 0
    |> (printfn "%A")

let second = 
    let rec iter r prev ls = 
        match ls with
        | e1::e2::e3::t -> iter (if prev < (e1 + e2 + e3) then r+1 else r) (e1+e2+e3) (e2::e3::t)
        | _ -> r

    getInputList
    |> iter 0 Int32.MaxValue
    |> (printfn "%A")