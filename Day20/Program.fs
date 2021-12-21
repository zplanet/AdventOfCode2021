open System
open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let toInt c = if '#' = c then 1 else 0

let enhancer = 
    input 
    |> Seq.head 
    |> Seq.fold (fun (idx, m) c -> (idx + 1, Map.add idx (toInt c) m)) (0, Map.empty) 
    |> snd

let image = 
    input 
    |> Seq.tail 
    |> Seq.tail
    |> Seq.fold
        (fun (r, m) line -> 
            (r + 1, 
            line
            |> Seq.fold (fun (c, m') ch -> (c + 1, Map.add (r,c) (toInt ch) m')) (0, m)
            |> snd))
        (0, Map.empty)
    |> snd

let perimeter =
    Map.fold 
        (fun (rs,re,cs,ce) (r,c) _ -> 
            ((if r < rs then r else rs), (if re < r then r else re), 
            (if c < cs then c else cs), (if ce < c then c else ce)))
        (Int32.MaxValue, Int32.MinValue, Int32.MaxValue, Int32.MinValue)

let calcIndex ls = 
    ls 
    |> List.zip [for x in (List.length ls - 1)..(-1)..0 -> (float)x] 
    |> List.map (fun (x, n) -> ((int)(2.0 ** x)) * n)
    |> List.sum

let enhance img =
    let expand = 4
    let (rs,re,cs,ce) = perimeter img
    [for r in (rs - expand)..(re + expand) do for c in (cs - expand)..(ce + expand) do (r,c)]
    |> List.fold
        (fun m (r,c) -> 
            let idx =
                [(-1,-1);(-1,0);(-1,1);(0,-1);(0,0);(0,1);(1,-1);(1,0);(1,1)]
                |> List.map 
                    (fun (r',c') -> 
                        match Map.tryFind (r + r', c + c') img with
                        | Some n -> n
                        | None -> 0)
                |> calcIndex
            Map.add (r,c) (enhancer.Item idx) m)
        Map.empty

let apply n img =
    let rec iter cnt img' =
        if cnt < n then
            iter 
                (cnt + 1) 
                (if cnt % 2 = 1 
                    then
                        let (rs,re,cs,ce) = perimeter img'
                        enhance img'
                        |> Map.filter (fun (r,c) _ -> rs < r && r < re && cs < c && c < ce)
                    else
                        enhance img')
        else img'
    iter 0 img

let show img = 
    let (rs,re,cs,ce) = perimeter img
    [for r in rs..re do 
        [|for c in cs..ce do
            match Map.tryFind (r,c) img with
            | Some n -> if r = 0 && c = 0 then '*' else (if 1 = n then '#' else '.')
            | None -> '.'|]]
    |> List.map String.Concat
    |> List.iter (printfn "%s")

image
|> apply 2
|> Map.fold (fun acc _ v -> acc + v) 0
|> printfn "part one: %A"

image
|> apply 50
|> Map.fold (fun acc _ v -> acc + v) 0
|> printfn "part two: %A"
