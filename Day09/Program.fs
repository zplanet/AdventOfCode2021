open System
open System.IO

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.map (fun line -> line |> Seq.toList |> List.map (fun c -> Int32.Parse(c.ToString())))
    |> Seq.toList

let partOne data =
    let pad ls =
        let paddedFrontBack = ls |> List.map (fun l -> List.append (9::l) [9])
        let extraRow = [ for i in 1 .. (List.head paddedFrontBack).Length -> 9 ]
        List.append (extraRow::paddedFrontBack) [extraRow]

    let filterLowPoints l1 l2 l3 =
        let isLowerH p v n = v < p && v < n
        let isLowerV u v d = v < u && v < d

        let td = List.zip l1 l3

        let rec iter r data =
            match data with
            | u1::u2::u3::utl, v1::v2::v3::vtl, d1::d2::d3::dtl -> 
                if (isLowerH v1 v2 v3) && (isLowerV u2 v2 d2) 
                then iter (v2::r) (u2::u3::utl, v2::v3::vtl, d2::d3::dtl)
                else iter r (u2::u3::utl, v2::v3::vtl, d2::d3::dtl)
            | _ -> r
        iter [] (l1, l2, l3)

    let rec iter r = function
        | r1::r2::r3::tl -> iter ((filterLowPoints r1 r2 r3)::r) (r2::r3::tl)
        | _ -> r

    data
    |> pad
    |> iter []
    |> List.concat
    |> List.map (fun x -> x + 1)
    |> List.sum
    |> printfn "part one: %A"


let partTwo data =
    let bfs rows =
        let rowCnt = rows |> List.length
        let colCnt = rows |> List.head |> List.length
        let coords = [ for r in 1 .. rowCnt -> [ for c in 1 .. colCnt -> (r, c) ] ] |> List.concat
        let map =
            rows
            |> List.concat
            |> List.zip coords
            |> List.fold (fun m (k, v) -> Map.add k v m) Map.empty

        let calcPath (ridx,cidx) visits =
            let rec iter size vs q =
                match q with
                | coord::t ->
                    match Set.contains coord vs with
                    | true -> iter size vs t
                    | _ -> 
                        match Map.tryFind coord map with
                        | Some v when 9 = v -> iter size vs t
                        | Some v -> 
                            iter 
                                (size+1) 
                                (Set.add coord vs) 
                                (List.append 
                                    t
                                    ([ (-1,0); (1,0); (0,-1); (0,1) ] 
                                    |> List.map (fun (a,b) -> (a+fst(coord), b+snd(coord)))))
                        | None -> iter size vs t
                | _ -> (size, vs)
            iter 0 visits [(ridx, cidx)]
            
        coords
        |> List.fold 
            (fun (sizes, visits) (ridx, cidx) -> 
                let (cnt, vs) = calcPath (ridx, cidx) visits
                (cnt::sizes, vs)
            ) 
            ([], Set.empty)
        |> fst
        |> List.filter (fun n -> n > 0)

    data
    |> bfs
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (fun a b -> a * b)
    |> printfn "part two: %A"

partOne input
partTwo input