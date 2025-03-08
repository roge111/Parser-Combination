module BasicParsers
open Parser


let satisfy predicate =
    Parser(fun input ->
        match input with
        | "" -> None
        | _ ->
            let c = input.[0]
            if predicate c then Some(c, input.[1..])
            else None)

let pChar c = satisfy ((=) c)


let pString (s: string) =
    Parser(fun input ->
        if input.StartsWith(s) then Some(s, input.[s.Length..])
        else None)

let rec many p =
    (p >>= fun x -> many p >>= fun xs -> returnP (x :: xs))
    <|> returnP []

let many1 p = p >>= fun x -> many p >>= fun xs -> returnP (x :: xs)
