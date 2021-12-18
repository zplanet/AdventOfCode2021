// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let lines = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList

let inputNumbers = lines |> List.head |> (fun s -> s.Split(',')) |> Seq.toList |> List.map (fun s -> Int32.Parse(s))

let toList input = (Regex.Matches(input, "[0-9]+")) |> Seq.toList |> List.map (fun m -> Int32.Parse(m.Value))

let applyCol f boards = boards |> List.map (fun board -> board |> List.map (fun row -> row |> List.map f))

let inputBoards = 
    lines 
    |> List.skip 2 
    |> List.map toList
    |> List.filter (fun l -> not (List.isEmpty l))
    |> List.fold 
        (fun (acc, tl, nth) elm -> 
            match nth with
            | n when 5 = n -> ((List.rev (elm::tl))::acc, [], 1)
            | n -> (acc, elm::tl, n + 1))
        ([], [], 1)
    |> (fun (acc, _, _) -> List.rev acc)
    |> applyCol (fun c -> (c, false))

let drawNumber n boards = boards |> applyCol (fun (v, b) -> if v = n || b then (v, true) else (v, false))

let checkBoard board =
    let isAnyRowBingo b = 
        b
        |> List.map (fun row -> List.forall (fun (_, b) -> b) row)
        |> List.reduce (fun b1 b2 -> b1 || b2)

    let isAnyColBingo b = 
        [0..4]
        |> List.map (fun n -> List.map (fun row -> snd (List.head (List.skip n row))) b)
        |> List.map (fun col -> List.forall (fun b -> b) col)
        |> List.reduce (fun b1 b2 -> b1 || b2)

    isAnyRowBingo board || isAnyColBingo board

let checkBoards boards =
    let rec iter bs = 
        if List.isEmpty bs 
        then 
            None
        else
            let board = List.head bs
            if checkBoard board 
            then Some(board)
            else iter (List.tail bs)
    iter boards

let sum elms = elms |> List.map (fun (t: int * bool) -> fst(t)) |> List.sum

let partOne boards numbers =
    let rec iter bs ns =
        if List.isEmpty ns 
        then 
            (0, [])
        else
            let n = List.head ns
            let nbs = drawNumber n bs
            match checkBoards nbs with
            | Some(board) -> (n, board)
            | None -> iter nbs (List.tail ns)

    let (lastNum, board) = iter boards numbers

    board
    |> List.concat
    |> List.partition (fun (v, b) -> b)
    |> (fun (_, unmarks) -> (sum unmarks) * lastNum)    

let checkBoards2 boards =
    let rec iter r bs lastBoard = 
        if List.isEmpty bs 
        then 
            lastBoard
        else
            let board = List.head bs
            if checkBoard board 
            then iter r (List.tail bs) (Some(List.append r (List.tail bs), board))
            else iter (List.append r [board]) (List.tail bs) lastBoard
    iter [] boards None

let partTwo boards numbers =
    let rec iter bs ns lastWin =
        if List.isEmpty ns 
        then 
            lastWin
        else
            let n = List.head ns
            let nbs = drawNumber n bs
            match checkBoards2 nbs with
            | Some(rbs, board) -> iter rbs (List.tail ns) (n, board)
            | None -> iter nbs (List.tail ns) lastWin

    let (num, board) = iter boards numbers (0, [])
    
    board
    |> List.concat
    |> List.partition (fun (v, b) -> b)
    |> (fun (_, unmarks) -> (sum unmarks) * num)

[<EntryPoint>]
let main argv =
    partOne inputBoards inputNumbers
    |> printfn "part one: %A"

    partTwo inputBoards inputNumbers
    |> printfn "part two: %A"

    // inputBoards
    // |> drawNumber 76
    // |> List.concat
    // |> List.concat
    // // //|> List.filter (fun (v, b) -> b)
    // |> List.filter (fun (v, b) -> v = 76)
    // |> printfn "%A"
    0 // return an integer exit code