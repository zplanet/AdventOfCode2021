open System
open System.IO
open System.Text.RegularExpressions

type Origami = FoldX of int | FoldY of int

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Coordinate|) str =
   let m = Regex.Match(str, "([0-9]+),([0-9]+)")
   if m.Success
   then Some (Int32.Parse(m.Groups[1].Value), Int32.Parse(m.Groups[2].Value))
   else None

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList

let coords = 
    input
    |> List.takeWhile (fun line -> line.Length > 0)
    |> List.map (function | Coordinate c -> c)
    |> List.choose id

let folds =
    input
    |> List.skipWhile (fun line -> line.Length > 0)
    |> List.map 
        (function 
        | ParseRegex "fold along x=([0-9]+)" [x;] -> Some (FoldX (Int32.Parse(x)))
        | ParseRegex "fold along y=([0-9]+)" [y;] -> Some (FoldY (Int32.Parse(y)))
        | _ -> None)
    |> List.choose id

let merge m1 m2 =
    m1
    |> Map.keys
    |> Seq.fold 
        (fun acc coord -> if Map.containsKey coord acc then acc else Map.add coord (m1.Item coord) acc)
        m2

let fold maxX maxY m = function
    | FoldX x -> 
        let (lefts, rights) = Map.partition (fun (cx,_) _ -> cx < x) m
        let rc = maxX - x
        let lc = x

        if lc < rc
        then
            let ls = lefts |> Map.keys |> Seq.fold (fun m' (cx,cy) -> Map.add ((maxX - x) - (x - cx), cy) 1 m') Map.empty
            let rs = rights |> Map.keys |> Seq.fold (fun m' (cx,cy) -> if x = cx then m' else Map.add ((maxX - x) - (cx - x), cy) 1 m') Map.empty
            merge ls rs
        else
            let rs = rights |> Map.keys |> Seq.fold (fun m' (cx,cy) -> if x = cx then m' else Map.add (x - (cx - x), cy) 1 m') Map.empty
            merge rs lefts

    | FoldY y -> 
        let (ups, downs) = Map.partition (fun (cx,cy) v -> cy < y) m
        let dc = maxY - y
        let uc = dc + 1

        if uc < dc
        then
            let us = ups |> Map.keys |> Seq.fold (fun m' (cx,cy) -> Map.add (cx, (maxY - y) - (y - cy)) 1 m') Map.empty
            let ds = downs |> Map.keys |> Seq.fold (fun m' (cx,cy) -> if y = cy then m' else  Map.add (cx, (maxY - y) - (cy - y)) 1 m') Map.empty
            merge us ds
        else
            let ds = downs |> Map.keys |> Seq.fold (fun m' (cx,cy) -> if y = cy then m' else Map.add (cx, y - (cy - y)) 1 m') Map.empty
            merge ds ups

let maxX = Map.keys >> Seq.maxBy fst >> fst
let maxY = Map.keys >> Seq.maxBy snd >> snd

let partOne coords folds =
    let map = coords |> List.fold (fun m coord -> Map.add coord 1 m) Map.empty
    fold (maxX map) (maxY map) map (List.head folds)

let partTwo coords folds =
    let map = coords |> List.fold (fun m coord -> Map.add coord 1 m) Map.empty
    let result = folds |> List.fold (fun m fld -> fold (maxX m) (maxY m) m fld) map

    let cx = maxX result
    let cy = maxY result

    [ 0 .. cy ]
    |> List.iter 
        (fun y -> 
            [ 0 .. cx ]
            |> List.map 
                (fun x -> 
                    match Map.tryFind (x,y) result with
                    | Some v -> '#'
                    | None -> '.')
            |> List.toArray
            |> String
            |> printfn "%A")

partOne coords folds
|> Map.count
|> printfn "%A"

partTwo coords folds
|> ignore
