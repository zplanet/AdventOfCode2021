open System.IO
open System.Text.RegularExpressions

let (|RuleExp|) s =
    let m = Regex.Match(s, "([A-Z]+) -> ([A-Z])")
    if m.Success then Some (m.Groups[1].Value, m.Groups[2].Value)
    else None

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList

let template = 
    input |> List.head |> Seq.toList

let rules =
    input |> List.tail |> List.tail
    |> List.map (function | RuleExp r -> r)
    |> List.choose id
    |> List.fold (fun m (p, c) -> Map.add (Seq.head p, Seq.head (Seq.tail p)) (Seq.head c) m) Map.empty

let step t = 
    let rec iter r = function
        | e1::e2::tl -> 
            match Map.tryFind (e1, e2) rules with
            | Some c -> iter (c::e1::r) (e2::tl)
            | None -> iter (e1::r) (e2::tl) 
        | e1::tl -> iter (e1::r) tl
        | [] -> r
    iter [] t
    |> List.rev

let steps n t =
    [ 1 .. n ]
    |> List.fold
        (fun acc _ -> step acc)
        t

let max = List.maxBy snd >> snd
let min = List.minBy snd >> snd

let calc n =
    template
    |> steps n
    |> List.groupBy id
    |> List.map (fun (c, ls) -> (c, List.length ls))
    |> (fun ls -> (max ls) - (min ls))

calc 10
|> printfn "part one: %A"
