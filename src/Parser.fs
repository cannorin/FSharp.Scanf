namespace FSharp.Scanf.Internals

open System
open System.ComponentModel

[<EditorBrowsable(EditorBrowsableState.Never)>]
module Parser =
  [<Literal>]
  let ParserChars = "bdisuxXoeEfFgGMcA"

  // Creates a list of formatter characters from a format string,
  // for example "(%s,%d)" -> ['s', 'd']
  let rec internal getFormatters xs =
    match xs with
    | '%' :: '%' :: xr -> getFormatters xr
    | '%' :: x :: xr   ->
      if ParserChars.Contains(string x) then x :: getFormatters xr
      else failwithf "Unsupported formatter '%%%c'" x
    | _ :: xr          -> getFormatters xr
    | []               -> []

  [<Struct; RequireQualifiedAccess>]
  type FormatStringPart =
    | Placeholder of typ:char
    | Literal of string
    | Space
    static member getFormatters(fmt: FormatStringPart list) =
      let rec get = function
        | Placeholder c :: rest -> c :: get rest
        | _ :: rest -> get rest
        | [] -> []
      get fmt

  let inline internal (<+>) h t =  
    match h, t with
      | FormatStringPart.Literal s, FormatStringPart.Literal t :: rest ->
        FormatStringPart.Literal (s+t) :: rest
      | _ -> h :: t

  let rec internal parsePlaceholderImpl currentPos (str: string) =
    let c = str.[currentPos]
    let nextPos = currentPos + 1
    if Char.IsLetter c then
      FormatStringPart.Placeholder c :: parseFormatImpl nextPos nextPos str
    else if c = '%' then
      FormatStringPart.Literal "%" <+> parseFormatImpl nextPos nextPos str
    else failwithf "Unsupported formatter '%%%c'" c

  and internal parseFormatImpl startPos currentPos (str: string) =
    if currentPos >= str.Length then
      if currentPos = startPos then []
      else
        let s = str.Substring(startPos, currentPos - startPos)
        FormatStringPart.Literal s :: []
    else
      let c = str.[currentPos]
      if c = '%' then
        let nextPos = currentPos + 1
        if currentPos = startPos then
          parsePlaceholderImpl nextPos str
        else
          let s = str.Substring(startPos, currentPos - startPos)
          FormatStringPart.Literal s <+> parsePlaceholderImpl nextPos str
      else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then
        let mutable i = 1
        while currentPos + i < str.Length
           && let c = str.[currentPos + i] in
           c = ' ' || c = '\n' || c = '\r' || c = '\t' do i <- i+1
        let nextPos = currentPos + i 
        if currentPos = startPos then
          FormatStringPart.Space :: parseFormatImpl nextPos nextPos str
        else
          let s = str.Substring(startPos, currentPos - startPos)
          FormatStringPart.Literal s :: FormatStringPart.Space :: parseFormatImpl nextPos nextPos str    
      else
        parseFormatImpl startPos (currentPos + 1) str

  let inline internal parseFormat (str: string) =
    parseFormatImpl 0 0 str

  open FParsec
  let inline internal (<++>) p1 p2 = p1 .>>. p2 |>> List.Cons
  let inline internal strOf p = withSkippedString (fun s _ -> s) p

  let pDecimal : Parser<string, unit> =
    numberLiteral (NumberLiteralOptions.AllowPlusSign ||| NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction) "decimal"
    |>> fun nl -> nl.String

  let rec internal buildParser = function
    | [] -> eof >>% []
    | FormatStringPart.Space :: rest ->
      spaces >>. buildParser rest
    | FormatStringPart.Literal lit :: rest ->
      skipString lit >>. buildParser rest
    | FormatStringPart.Placeholder c :: rest->
      let cont = buildParser rest
      match c with
        | 'b' -> (pstring "true" <|> pstring "false") <++> cont
        | 'd' | 'i' -> strOf pint64 <++> cont
        | 's' | 'A' ->
          manyCharsTill anyChar (followedBy cont) .>>.? cont |>> List.Cons
        | 'u' -> strOf puint64 <++> cont
        | 'x' | 'X' -> manySatisfy isHex <++> cont
        | 'o' -> manySatisfy isOctal <++> cont
        | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' ->
          (skipStringCI "nan" >>% "NaN")
          <|> (skipStringCI "infinity" <|> skipStringCI "inf" >>% "Infinity")
          <|> (strOf pfloat)
          <++> cont
        | 'M' -> pDecimal <++> cont
        | 'c' -> anyChar |>> string <++> cont
        | c -> failwithf "Unsupported formatter '%%%c'" c

  open System.Collections.Generic
  let private parserCache = new Dictionary<string, FormatStringPart list * Parser<string list, unit>>()

  let internal cached (pf: PrintfFormat<_, _, _, _, _>) =
    let formatStr  = pf.Value
    match parserCache.TryGetValue(formatStr) with
    | true, x  -> x
    | false, _ ->
      let fsp = parseFormat formatStr
      let p = buildParser fsp
      parserCache.[formatStr] <- (fsp, p)
      (fsp, p)
  
  // Extracts string matches and the format from a format string and a given string.
  let getMatchesAndFormat (pf: PrintfFormat<_, _, _, _, _>) s =
    let fmt, parser = cached pf
    match run parser s with
    | Success (groups, _, _) ->
      let formatters = FormatStringPart.getFormatters fmt
      groups, formatters
    | Failure (msg, _, _) ->
      failwithf "the input does not match the format '%s': %s" pf.Value msg

  type PrintfFormat<'a,'b,'c,'d,'e> with
    member this.GetFormatterNames () =
      let fs = this.Value.ToCharArray()
               |> Array.toList |> getFormatters in
      let print = function
        | 's' -> "string"
        | 'c' -> "char"
        | 'b' -> "bool"
        | 'i' | 'd' -> "int"
        | 'u' -> "uint"
        | 'x' -> "lowercase hex"
        | 'X' -> "uppercase hex"
        | 'o' -> "octal"
        | 'f' | 'e' | 'E' | 'g' | 'G' -> "double"
        | 'M' -> "decimal"
        | 'A' -> "any type"
        | x -> failwithf "Unsupported formatter '%%%c'" x
      in
      fs |> List.map print

    member this.PrettyTokenize names =
      let fcs = this.Value.ToCharArray() |> Array.toList in
      if (List.length names) < (fcs |> getFormatters |> List.length) then
        failwith "Parameter count does not match to the format"
      else
        let rec replace = function
          | [], _ -> []
          | cs, [] ->
            cs |> List.map string
          | '%' :: '%' :: cs, ns ->
            replace (cs, ns)
          | '%' :: c :: cs, n :: ns when ParserChars.Contains(string c) ->
            n :: replace (cs, ns)
          | c :: cs, ns ->
            string c :: replace (cs, ns)
        in
        replace (fcs, names)

    member this.PrettyPrint names = this.PrettyTokenize names |> String.concat ""
