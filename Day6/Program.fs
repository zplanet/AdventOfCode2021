// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics

let initialState = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.head
    |> (fun line -> Seq.toList (line.Split(',')))
    |> List.map (fun s -> Int32.Parse(s))

let afterDays timer days =
    let cache = new Dictionary<_,_>()
    let calcNewFishes days =
        match cache.TryGetValue days with
        | true, v -> v
        | false, _ ->
            let d = (days / 7) - (if 0 = days % 7 then 1 else 0)
            let r = (9, days)::[ for i in 1 .. d -> (9, days - 7 * i)]
            cache.Add(days, (List.rev r))
            r
    let rec iter r ls =
        match ls with
        | (tmr, ds)::tl when tmr < ds -> iter (r+(1UL)) (List.append (calcNewFishes (ds - tmr)) tl)
        | (tmr, ds)::tl when ds <= tmr -> iter (r+(1UL)) tl
        | _ -> r
    iter (0UL) [(timer, days)]

let runWithTimer f = 
    let sw = new Stopwatch()
    sw.Start()
    f initialState
    sw.Stop()
    printfn "elapsed time: %A" (sw.Elapsed.ToString())

[<EntryPoint>]
let main argv =
    
    runWithTimer (fun state ->
        state
        |> List.map (fun timer -> afterDays timer 80)
        |> List.sum
        |> printfn "part one: %A"
    )

    runWithTimer (fun state ->
        let cache = new Dictionary<_,_>()

        state
        |> List.map 
            (fun timer -> 
                match cache.TryGetValue timer with
                | true, r -> r
                | false, _ -> 
                    let r = afterDays timer 256
                    cache.Add(timer, r)
                    r)
        |> List.sum
        |> printfn "part two: %A"
    )
    
    0 // return an integer exit code