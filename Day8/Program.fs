open System.IO

let input =
    let toSet = List.map (fun s -> Set(s))
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList
    |> List.map (fun line -> line.Split('|'))
    |> List.map (fun arr -> Array.toList (arr[0].Trim().Split(' ')), Array.toList (arr[1].Trim().Split(' ')))
    |> List.map (fun (ps, os) -> toSet ps, toSet os)

let partOne =
    input
    |> List.map 
        (fun (_, outputs) -> 
            outputs 
            |> List.filter (fun s -> 2 = s.Count || 4 = s.Count || 3 = s.Count || 7 = s.Count )
            |> List.length)
    |> List.sum
    |> printfn "part one: %A"

let partTwo =
    let decode (patterns, outputs) =
        let flip m =
            m
            |> Map.toList
            |> List.fold (fun acc (k, v) -> Map.add v k acc) Map.empty<Set<char>, int>

        let rec iter m ls = 
            match ls with
            | s::t -> 
                match Set.count s with
                | 2 -> iter (Map.add 1 s m) t
                | 4 -> iter (Map.add 4 s m) t
                | 3 -> iter (Map.add 7 s m) t
                | 7 -> iter (Map.add 8 s m) t
                | 5 when (Map.containsKey 1 m) && (Set.isSubset (m.Item 1) s) -> iter (Map.add 3 s m) t
                | 5 when (Map.containsKey 8 m) && (Map.containsKey 4 m) && (Set.isSubset ((m.Item 8) - (m.Item 4)) s) -> iter (Map.add 2 s m) t
                | 5 when (Map.containsKey 2 m) && (Map.containsKey 3 m) -> iter (Map.add 5 s m) t
                | 6 when (Map.containsKey 4 m) && (Set.isSubset (m.Item 4) s) -> iter (Map.add 9 s m) t
                | 6 when (Map.containsKey 8 m) && (Map.containsKey 7 m) && (Set.isSubset ((m.Item 8) - (m.Item 7)) s) -> iter (Map.add 6 s m) t
                | 6 when (Map.containsKey 6 m) && (Map.containsKey 9 m) -> iter (Map.add 0 s m) t
                | _ -> iter m (List.append t [s])
            | _ -> m

        let patternMap = 
            patterns
            |> iter Map.empty<int, Set<char>>
            |> flip

        outputs
        |> List.map (fun s -> patternMap.Item s)
        |> List.zip [1000; 100; 10; 1]
        |> List.map (fun (a, b) -> a * b)
        |> List.sum

    input
    |> List.map decode
    |> List.sum
    |> printfn "part two: %A"
    