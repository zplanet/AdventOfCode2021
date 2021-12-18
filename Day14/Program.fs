open System.IO
open System.Text.RegularExpressions

let (|RuleExp|) s =
    let m = Regex.Match(s, "([A-Z]+) -> ([A-Z])")
    if m.Success then Some (m.Groups[1].Value, m.Groups[2].Value)
    else None

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let template = input |> Seq.head

let rules =
    input |> Seq.tail |> Seq.tail
    |> Seq.map (function | RuleExp r -> r)
    |> Seq.choose id
    |> Seq.fold (fun m (p, c) -> Map.add (Seq.head p, Seq.head (Seq.tail p)) (Seq.head c) m) Map.empty

let max = Seq.maxBy snd >> snd
let min = Seq.minBy snd >> snd

let add cnt pair m =
    Map.change pair (function | Some n -> Some (n + cnt) | None -> Some (cnt)) m

let calc n =
    let rec iter cnt m =
        if cnt < n then
            m 
            |> Map.toSeq
            |> Seq.fold 
                (fun m' ((p1,p2), cnt) -> 
                    match Map.tryFind (p1,p2) rules with
                    | Some c -> m' |> add cnt (p1,c) |> add cnt (c,p2)
                    | None -> m')
                Map.empty
            |> iter (cnt+1) 
        else 
            m
            |> Map.toSeq
            |> Seq.map (fun ((p1,_),cnt) -> (p1,cnt))
            |> Seq.fold (fun acc (c,n) -> Map.change c (function | Some v -> Some (n+v) | None -> Some n) acc) Map.empty
            |> Map.change (Seq.last template) (function | Some n -> Some (n+1UL) | None -> None)
            |> Map.toSeq
            |> (fun l -> (max l) - (min l))

    template
    |> Seq.pairwise
    |> Seq.fold (fun m pair -> add 1UL pair m) Map.empty
    |> iter 0

calc 10
|> printfn "part one: %A"

calc 40
|> printfn "part two: %A"
