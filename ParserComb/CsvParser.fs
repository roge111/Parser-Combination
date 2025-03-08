module CsvParser
open Parser
open BasicParsers


let parseCell =
    many (satisfy (fun c -> c <> ';' && c <> '\n'))
    |> mapP (fun chars -> chars |> List.toArray |> System.String)

let parseCsvLine =
    let comma = pChar ';'
    parseCell >>= fun firstCell ->
    many (comma >>= fun _ -> parseCell) >>= fun restCells ->
    returnP (firstCell :: restCells)
