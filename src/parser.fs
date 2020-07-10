namespace Loglo


// ----------------------------------------------------------------------------
// PARSER
// ----------------------------------------------------------------------------
module Parser =
  open Parsec
  type private Char = System.Char // avoid open System


  type Expr =
    | Reference of (char*int)
    | IndirectRef
    | NestedRef of (char*int)*int
    | Number of int
    | Symbol of string
    | SymbolLiteral of string
    | TextExp of string
    | Spaces of int
    | Block of Expr list
    | Unparsable of string

  let private spaceChar =
      let predicate = (fun c -> c = ' ')
      let label = "space"
      satisfy predicate label

  let private spaces =
    many spaceChar

  let private spaces1 =
    many1 spaceChar

  let private anyChar =
    let predicate = (fun c -> c <> '\n' ) //Parsec adds /n
    let label = "anything"
    satisfy predicate label

  let private anyString1 =
    many1 anyChar

  let private literalString =
    let lChar = anyOf (['A'..'Z']@['a'..'z']@['*'; '+'; '-'; '/']@['0'..'9'])
    between (pchar '"') (many lChar) (pchar '"')

  let private refString =
    let rChar = anyOf(['A'..'Z']@['a'..'z'])
    rChar .>>. pint

  let private reference =
    refString |> mapP (fun (c, i) -> Reference (Char.ToUpper c, i))

  let private nestedRef =
    refString .>>. ((pchar ':' .>>. pchar ':') >>. pint)
    |> mapP (fun ((c, i), j) -> NestedRef ((Char.ToUpper c, i), j))

  let private indirectRef =
    pchar '^' |> mapP (fun _ -> IndirectRef)

  let private symbolString =
    let sChar = anyOf (['A'..'Z']@['a'..'z']@['*'; '+'; '-'; '/'])
    sChar .>>. many sChar |> mapP (fun (a, b) -> charListToStr (a::b))

  let private symbol =
    symbolString |> mapP (fun s -> Symbol s)

  let private symbolLiteral =
    pchar ':' >>. symbolString |> mapP (fun s -> SymbolLiteral s)

  let private number =
    pint |> mapP Number

  let private ws =
    spaces1 |> mapP (fun s -> Spaces s.Length)

  let private text =
    literalString |> mapP (fun l -> TextExp (charListToStr l))

  let private unparsable =
    anyString1 |> mapP (fun l -> Unparsable (charListToStr l))

  let private blockExprs =
    let exprs = many (choice [reference; indirectRef; symbol; ws; number; text])
    between (pchar '{') exprs (pchar '}')

  let private block =
    blockExprs |> mapP (fun l -> Block  l)

  let private expressionList' =
    many (choice [ nestedRef; reference; indirectRef; symbol; ws; number; text; block; symbolLiteral; unparsable])

  let parse input =
    match run expressionList' input with
    | Success (expList, _) ->
        expList
        |> List.filter (fun e -> match e with | Spaces _ -> false | _ -> true)
        |> Ok
    | Failure (f, c, d) ->
        Error (f.ToString(), c.ToString())
