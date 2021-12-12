open System.IO

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList
    |> List.map 
        (fun line -> 
            let nodes = line.Split('-')
            (Array.head nodes, Array.head (Array.tail nodes)))

let toMap ls =
    let add k v m =
        Map.change 
            k
            (function
            | Some ls -> Some (v::ls)
            | None -> Some [v])
            m
    ls
    |> List.fold 
        (fun m (a,b) -> 
            m 
            |> (fun m' -> if "end" = a || "start" = b then m' else add a b m')
            |> (fun m' -> if "start" = a || "end" = b then m' else add b a m')) 
        Map.empty

let isLowerCase = (<) "ZZ"

let partOne map =
    let update k m =
        if isLowerCase k then
            Map.remove k m
        else
            m
    let rec traverse node m =
        if "end" = node then 1
        else
            match Map.tryFind node m with
            | Some nodes -> nodes |> List.map (fun n -> traverse n (update node m)) |> List.sum
            | None -> 0
    traverse "start" map

let partTwo map =
    let checkAndSum node m f = 
        match Map.tryFind node m with
        | Some nodes -> nodes |> List.map f |> List.sum
        | None -> 0
    let rec traverse node visits found m =
        if "end" = node 
        then 1
        else
            match isLowerCase node, Set.contains node visits, found with
            | true, true, true -> 0
            | true, true, false -> checkAndSum node m (fun n -> traverse n visits true (Map.remove node m))
            | true, false, _ -> checkAndSum node m (fun n -> traverse n (Set.add node visits) found m)
            | false, _, _ -> checkAndSum node m (fun n -> traverse n visits found m)
    traverse "start" Set.empty false map

input
|> toMap
|> partOne
|> printfn "part one: %A"

input
|> toMap
|> partTwo
|> printfn "part two: %A"
