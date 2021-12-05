// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let coordinates =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList
    |> List.map (fun line -> 
        let m = Regex.Match(line, "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)")
        [ for g in m.Groups -> g.Value ] |> List.tail)
    |> List.choose (function
        | [x1;y1;x2;y2] -> Some((Int32.Parse(x1),Int32.Parse(y1)),(Int32.Parse(x2),Int32.Parse(y2)))
        | _ -> None)

let toCoordinates = function
    | ((x1,y1),(x2,y2)) when x1 = x2 && y1 = y2 -> [ (x1, y1) ]
    | ((x1,y1),(x2,y2)) when x1 = x2 && y1 < y2 -> [ for y in y1 .. y2 -> (x1, y) ]
    | ((x1,y1),(x2,y2)) when x1 = x2 && y2 < y1 -> [ for y in y2 .. y1 -> (x1, y) ]
    | ((x1,y1),(x2,y2)) when y1 = y2 && x1 < x2 -> [ for x in x1 .. x2 -> (x, y1) ]
    | ((x1,y1),(x2,y2)) when y1 = y2 && x2 < x1 -> [ for x in x2 .. x1 -> (x, y1) ]
    | ((x1,y1),(x2,y2)) when x1 < x2 && y1 < y2 -> List.zip [ x1 .. x2 ] [ y1 .. y2 ]
    | ((x1,y1),(x2,y2)) when x1 < x2 && y2 < y1 -> List.zip [ x1 .. x2 ] [ y1 .. -1 .. y2 ]
    | ((x1,y1),(x2,y2)) when x2 < x1 && y1 < y2 -> List.zip [ x2 .. x1 ] [ y2 .. -1 .. y1 ]
    | ((x1,y1),(x2,y2)) when x2 < x1 && y2 < y1 -> List.zip [ x2 .. x1 ] [ y2 .. y1 ]
    | _ -> []

let calc predicate coords =
    coords
    |> List.filter predicate
    |> List.map toCoordinates
    |> List.concat
    |> List.groupBy (fun coord -> coord)
    |> List.filter (fun (key, elms) -> 1 < (List.length elms))
    |> List.length

let partOne = calc (fun ((x1,y1),(x2,y2)) -> x1 = x2 || y1 = y2)
let partTwo = calc (fun _ -> true)

[<EntryPoint>]
let main argv =

    coordinates
    |> partOne
    |> printfn "part one: %A"

    coordinates
    |> partTwo
    |> printfn "part two: %A"

    0 // return an integer exit code