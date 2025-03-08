module Parser


type Parser<'T> = Parser of (string -> ('T * string) option)


let runParser (Parser p) input = p input


let returnP x = Parser(fun input -> Some(x, input))


let bindP p f =
    Parser(fun input ->
        match runParser p input with
        | Some(result, rest) -> runParser (f result) rest
        | None -> None)


let (>>=) = bindP

let mapP f p = p >>= (f >> returnP)

let (<|>) p1 p2 =
    Parser(fun input ->
        match runParser p1 input with
        | Some _ as result -> result
        | None -> runParser p2 input)
