open System
open JsonParser
open CsvParser
open Parser

[<EntryPoint>]
let main argv =
    printfn "Тесты JSON:"
    let jsonTestCases = ["null"; "true"; "-123.45"; "\"Hello, F#\""]
    jsonTestCases |> List.iter (fun input ->
        match runParser jsonValue input with
        | Some(result, _) -> printfn "Парсинг JSON: %s -> %A" input result
        | None -> printfn "Ошибка парсинга JSON: %s" input)

    printfn "\nТест CSV:"
    let csvInput = "val1;val2;val3\n"
    match runParser parseCsvLine csvInput with
    | Some(result, _) -> printfn "Парсинг CSV: %A" result
    | None -> printfn "Ошибка парсинга CSV"

    0
