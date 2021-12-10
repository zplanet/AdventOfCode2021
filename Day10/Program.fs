open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList
    |> List.map (fun line -> line |> Seq.toList)

let partOne data =
    let checkCorrupted symbols =
        let rec iter r ls =
            match ls with
            | h::t -> 
                match h with
                | c when c = ')' -> if (List.head r) = '(' then iter (List.tail r) t else Some 3
                | c when c = ']' -> if (List.head r) = '[' then iter (List.tail r) t else Some 57
                | c when c = '}' -> if (List.head r) = '{' then iter (List.tail r) t else Some 1197
                | c when c = '>' -> if (List.head r) = '<' then iter (List.tail r) t else Some 25137
                | c -> iter (c::r) t
            | _ -> None
        iter [] symbols

    data
    |> List.map checkCorrupted
    |> List.choose id
    |> List.sum
    |> printfn "part one: %A"

let partTwo data =
    let checkIncomplete symbols =
        let rec iter r ls =
            match ls with
            | h::t -> 
                match h with
                | c when c = ')' -> if (List.head r) = '(' then iter (List.tail r) t else None
                | c when c = ']' -> if (List.head r) = '[' then iter (List.tail r) t else None
                | c when c = '}' -> if (List.head r) = '{' then iter (List.tail r) t else None
                | c when c = '>' -> if (List.head r) = '<' then iter (List.tail r) t else None
                | c -> iter (c::r) t
            | _ -> Some r
        iter [] symbols

    let makeComplete symbols =
        let rec iter r ls =
            match ls with
            | h::t -> 
                match h with
                | c when c = '(' -> iter (1UL::r) t
                | c when c = '[' -> iter (2UL::r) t
                | c when c = '{' -> iter (3UL::r) t
                | _ -> iter (4UL::r) t // '<'
            | _ -> r
        iter [] symbols

    let calcScore points = points |> List.fold (fun acc pt -> acc * 5UL + pt) 0UL

    let result =
        data
        |> List.map checkIncomplete
        |> List.choose id
        |> List.map List.rev
        |> List.map makeComplete
        |> List.map calcScore
        |> List.sort

    result
    |> List.skip ((List.length result) / 2)
    |> List.head
    |> printfn "part two: %A"

partOne input
partTwo input