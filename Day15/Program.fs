open System
open System.IO

let lines =
    Path.Combine(Directory.GetCurrentDirectory(), "input.test.txt")
    |> File.ReadLines

let length = Seq.length lines

type Coord = Coord of int * int
type Result = { Visited:bool; Cost: int; From: Coord }

let input =    
    lines
    |> Seq.map (fun line -> line |> Seq.map (fun ch -> Int32.Parse(ch.ToString())))
    |> Seq.concat
    |> Seq.zip (seq { for y in 1..length do for x in 1..length do yield (y,x) })
    |> Seq.fold (fun m (coord,v) -> Map.add (Coord coord) v m) Map.empty

let calc map =
    
    let next = Map.toSeq >> (Seq.minBy (snd >> (fun r -> (r.Visited,r.Cost))))

    let update (coord, result) = 
        Map.change 
            coord 
            (function
            | Some r -> 
                Some (if result.Cost < r.Cost then { r with Cost=result.Cost; From=result.From } else r)
            | None -> Some result)

    let setVisited coord = 
        Map.change coord (function | Some r -> Some { r with Visited=true } | None -> None)

    let rec iter matrix table =
        let (Coord (cr, cc), res) = table |> next

        if res.Visited then table
        else
            [(-1,0);(1,0);(0,-1);(0,1)]
            |> List.map 
                (fun (r,c) -> 
                    let nc = Coord (r + cr, c + cc)
                    match Map.tryFind nc matrix with
                    | Some risk -> Some (nc, { Visited=false; Cost=(res.Cost + risk); From=Coord (cr, cc); })
                    | None -> None)
            |> List.choose id
            |> List.fold (fun t r -> update r t) (setVisited (Coord (cr, cc)) table)
            |> iter matrix

    iter map (Map.empty.Add(Coord (1, 1), { Visited=false; Cost=0; From=(Coord (1, 1)) }))

let x5 map =
    let nextRisk cr d = 
        let nr = (cr + d) % 9
        if 0 = nr then 9 else nr
    let next dr dc drisk =
        Map.fold 
            (fun s (Coord (cr,cc)) risk -> Map.add (Coord (cr+dr,cc+dc)) (nextRisk risk drisk) s) 
            Map.empty

    [0..4]
    |> List.map (fun n -> next (length * n) 0 n map)
    |> List.map (fun m -> [0..4] |> List.map (fun n -> next 0 (length * n) n m))
    |> List.concat
    |> List.fold (fun s m -> Map.fold (fun acc k v -> Map.add k v acc) s m) Map.empty

// input
// |> calc
// |> (fun table -> 
//     match Map.tryFind (Coord (length, length)) table with
//     | Some v -> Some v
//     | None -> None)
// |> printfn "part one: %A"

input
|> x5
|> calc
|> (fun table -> 
    match Map.tryFind (Coord (length * 5, length * 5)) table with
    | Some v -> Some v
    | None -> None)
|> printfn "part two: %A"
