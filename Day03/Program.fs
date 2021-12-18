// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let getInputList = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.map (fun line -> Convert.ToInt32(line, 2))
    |> Seq.toList

let length = 12
let indexes = [11..-1..0]
let pow2 n = (int)(2.0 ** (float)n)
let indexWithMumtiples2 = indexes |> List.map (fun i -> (i, pow2 i))

let addList l1 l2 = List.zip l1 l2 |> List.map (fun t -> fst(t) + snd(t))
let toList n = indexWithMumtiples2 |> List.map (fun (i, m) -> (n &&& m) >>> i)
let toInt ns = List.zip indexes ns |> List.map (fun (i, n) -> n <<< i) |> List.sum

let gamma = 
    getInputList
    |> List.fold (fun (cnt, acc) elm -> (cnt + 1, addList acc (toList elm))) (0, List.init length (fun n -> 0))
    |> (fun (cnt, ones) -> 
            let zeros = List.map (fun n -> cnt - n) ones
            List.zip ones zeros
            |> List.map (fun (one, zero) -> if one < zero then 0 else 1))

let epsilon = List.map (fun n -> if n = 0 then 1 else 0) gamma

let calcRates choose = 
    let rec iter iwms ns =
        if List.isEmpty iwms || 1 = (List.length ns)
        then ns
        else
            let (i, m) = List.head iwms
            let (ones, zeros) = List.partition (fun n -> 1 = ((n &&& m) >>> i)) ns
            iter (List.tail iwms) (choose ones zeros)
    iter indexWithMumtiples2 getInputList

let oxygen = calcRates (fun ones zeros -> if (List.length ones) < (List.length zeros) then zeros else ones) |> List.head

let co2 = calcRates (fun ones zeros -> if (List.length ones) < (List.length zeros) then ones else zeros) |> List.head

[<EntryPoint>]
let main argv =
    
    (toInt gamma) * (toInt epsilon)
    |> printfn "first: %A"

    (oxygen * co2)
    |> printfn "second: %A"
    
    0 // return an integer exit code