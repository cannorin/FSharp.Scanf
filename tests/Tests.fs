module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FSharpPlus

open FSharp.Scanf

let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let inline testN fmt input =
  let str = uncurryN (sprintf fmt) input
  let output = sscanf fmt str
  input .=. output

let [<Property>] ``scanf of int works`` (i: int) =
  let str = sprintf "%i" i
  let value = sscanf "%i" str
  i .=. value

let [<Property>] ``scanf of 4 ints works`` input = testN "%i,%i,%i,%i" input
let [<Property>] ``scanf of 8 ints works`` input = testN "%i,%i,%i,%i,%i,%i,%i,%i" input
let [<Property>] ``scanf of 12 ints works`` input = testN "%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i" input
let [<Property>] ``scanf of 16 ints works`` input = testN "%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i" input

let [<Property>] ``scanf of octal works`` input = testN "%o,%o" input
let [<Property>] ``scanf of hex works`` input = testN "%x,%x" input

let isAscii ch = Char.IsLetterOrDigit(ch) || Char.IsPunctuation(ch) || Char.IsSymbol(ch) || (ch=' ')

let isOk (c: char) = c <> '\n' && c <> '\r'
let [<Property>] ``scanf of non-newline char works`` (c1, c2) =
  (isOk c1 && isOk c2) ==> testN "%c,%c" (c1, c2)

let [<Property>] ``scanf of bool works`` input = testN "%b,%b" input

let checkFloat fmt input =
  let str : string = sprintf fmt input
  let a = float (str.Replace("infinity", "Infinity"))
  let b = sscanf fmt str
  (abs (a - b) < Double.Epsilon) |@ (sprintf "|%f - %f| < eps" a b)
  .|. (Double.IsNaN a && Double.IsNaN b) |@ "nan"
  .|. (a .=. b)

let [<Property>] ``scanf of %f float works`` input = checkFloat "%f" input
let [<Property>] ``scanf of %e float works`` input = checkFloat "%e" input
let [<Property>] ``scanf of %g float works`` input = checkFloat "%g" input

let [<Property>] ``scanf of a decimal works`` input = testN "%M,%M" input

let normNewline (s: string) =
  s.Replace("\r\n", "\n")
   .Replace("\r", "\n")

let [<Property>] ``scanf of a string works`` (NonNull input) =
  sscanf "%s" input .=. normNewline input

type A = A of string with
  static member Convert (s: string, _: char, _: A) = A s

let [<Property>] ``scanf of custom type works`` (NonNull input) =
  let value : A = sscanf "%A" input
  value .=. A (normNewline input)

