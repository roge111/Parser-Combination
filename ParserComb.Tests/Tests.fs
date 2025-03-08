module Tests

open Expecto
open JsonParser
open CsvParser
open Parser

[<Tests>]
let jsonTests =
  testList "Тесты JSON-парсера" [
    testCase "Парсинг null" <| fun () ->
      match runParser jsonValue "null" with
      | Some(result, _) -> Expect.equal result null "Ожидалось значение null"
      | None -> failtest "Ошибка парсинга 'null'"
      
    testCase "Парсинг true" <| fun () ->
      match runParser jsonValue "true" with
      | Some(result, _) -> Expect.equal result true "Ожидалось значение true"
      | None -> failtest "Ошибка парсинга 'true'"
      
    testCase "Парсинг числа" <| fun () ->
      match runParser jsonValue "-123.45" with
      | Some(result, _) -> Expect.floatClose Accuracy.high (result :?> float) (-123.45) "Ожидалось число -123.45"
      | None -> failtest "Ошибка парсинга числа"
      
    testCase "Парсинг строки" <| fun () ->
      match runParser jsonValue "\"Hello, F#\"" with
      | Some(result, _) -> Expect.equal (result :?> string) "Hello, F#" "Ожидалась строка \"Hello, F#\""
      | None -> failtest "Ошибка парсинга строки"
  ]

[<Tests>]
let csvTests =
  testList "Тесты CSV-парсера" [
    testCase "Парсинг строки CSV" <| fun () ->
      let input = "val1;val2;val3\n"
      match runParser parseCsvLine input with
      | Some(result, _) -> Expect.sequenceEqual result ["val1"; "val2"; "val3"] "Ожидался правильный разбор CSV строки"
      | None -> failtest "Ошибка парсинга строки CSV"
  ]

[<Tests>]
let allTests =
  testList "Все тесты" [ jsonTests; csvTests ]


let main argv =
  runTestsInAssembly defaultConfig argv
