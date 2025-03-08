module JsonParser
open Parser
open BasicParsers


let whitespace = many (satisfy System.Char.IsWhiteSpace) |> mapP ignore

let jNull = pString "null" |> mapP (fun _ -> null: obj)


let jBool =
    (pString "true" |> mapP (fun _ -> true))
    <|> (pString "false" |> mapP (fun _ -> false))


let jNumber =
    many1 (satisfy (fun c -> System.Char.IsDigit c || c = '.' || c = '-'))
    |> mapP (fun chars -> chars |> List.toArray |> System.String |> float)


let jString =
    let quote = pChar '"'
    let strChar = satisfy ((<>) '"')
    quote >>= fun _ ->
    many strChar >>= fun chars ->
    quote >>= fun _ ->
    returnP (chars |> List.toArray |> System.String)


let jsonValue =
    (jNull |> mapP box)
    <|> (jBool |> mapP box)
    <|> (jNumber |> mapP box)
    <|> (jString |> mapP box)
