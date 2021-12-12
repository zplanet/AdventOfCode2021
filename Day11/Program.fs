open System
open System.IO

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList
    |> List.map (fun line -> Seq.toList line)
    |> List.map (fun ls -> ls |> List.map (fun c -> Int32.Parse(c.ToString())))

let coords = 
    [ for i in 1 .. 10 -> [ for j in 1 .. 10 -> (i,j) ] ]
    |> List.concat

let toMap matrix =    
    matrix
    |> List.concat
    |> List.zip coords
    |> List.fold (fun m (coord, v) -> Map.add coord v m) Map.empty

let initCell coord map = 
    Map.change 
        coord 
        (fun v -> 
            match v with
            | Some _ -> Some 0
            | None -> None) 
        map

let increase map =
    coords
    |> List.fold 
        (fun (m, r) coord -> 
            let nm = Map.change coord (function | Some n -> Some (n + 1) | None -> None) m
            if 9 < (nm.Item coord) then (initCell coord nm, coord::r)
            else (nm, r)
        )
        (map, [])

let increaseNeighbor (map,r) coord =
    let nm = 
        Map.change 
            coord 
            (function 
            | Some n when 0 = n -> Some 0
            | Some n -> Some (n + 1)
            | None -> None) 
            map
    if 9 < (nm.Item coord) then (initCell coord nm, coord::r)
    else (nm, r)

let getNeighbors (r,c) =
    [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]
    |> List.map (fun (a,b) -> (r+a, c+b))

let calc map =
    let rec iter (m, cs) =
        if List.isEmpty cs then m
        else 
            cs
            |> List.map getNeighbors
            |> List.concat
            |> List.filter (fun coord -> Map.containsKey coord m)
            |> List.fold increaseNeighbor (m, [])
            |> iter
    map
    |> increase
    |> iter

let partOne n map =
    [ 1 .. n ]
    |> List.fold 
        (fun (m, total) _ -> 
            let nm = m |> calc
            (nm, total + (nm |> Map.values |> Seq.filter (fun n -> 0 = n) |> Seq.length)))
        (map, 0)
    |> snd
    |> printfn "part one: %A"

let partTwo map =
    let rec iter cnt m =
        if (m |> Map.values |> Seq.forall (fun n -> 0 = n)) then cnt
        else iter (cnt + 1) (calc m)
    iter 0 map
    |> printfn "part two: %A"

input
|> toMap
|> partOne 100

input
|> toMap
|> partTwo 
