namespace FSharp.Scanf.Internals

open System
open System.ComponentModel

#nowarn "0042" // retype

[<EditorBrowsable(EditorBrowsableState.Never)>]
module OptimizedConverter =
  let inline whenNestedTuple (t: 't) = 
    (^t: (member Item1: 't1) t), (^t: (member Item2: 't2) t), (^t: (member Item3: 't3) t), (^t: (member Item4: 't4) t), (^t: (member Item5: 't5) t), (^t: (member Item6: 't6) t), (^t: (member Item7: 't7) t), (^t: (member Rest: 'tr) t)

  let inline retype (x: 'T) : 'U = (# "" x: 'U #)

  let inline check f x = if f x then x else failwithf "format failure \"%s\"" x
  let inline parseDecimal x = Decimal.Parse(x, Globalization.CultureInfo.InvariantCulture)

  let formatIntegerStr str fmt =
    match fmt with
      | 'i' | 'd' | 'u' -> str
      | 'x' -> str |> check (String.forall (fun c -> Char.IsLower c || Char.IsDigit c)) |> ((+) "0x")
      | 'X' -> str |> check (String.forall (fun c -> Char.IsUpper c || Char.IsDigit c)) |> ((+) "0x")
      | 'o' -> "0o" + str
      | _ -> str

  type ConvertOne =
    static member inline Invoke (s: string) (f: char) =
      let inline call_2 (_a: ^a, b: ^b) = ((^a or ^b): (static member Convert: _*_*_->_) s,f,b)
      let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'t>) : 't
      call Unchecked.defaultof<ConvertOne>

    static member inline Convert (_: string, _: char, _t: unit)    = ()
    static member inline Convert (s: string, _: char, _t: bool)    = Boolean.Parse(s)
    static member inline Convert (s: string, _: char, _t: string)  = s
    static member inline Convert (s: string, _: char, _t: char)    = char s
    static member inline Convert (s: string, f: char, _t: int8)    = formatIntegerStr s f |> int8
    static member inline Convert (s: string, f: char, _t: uint8)   = formatIntegerStr s f |> uint8
    static member inline Convert (s: string, f: char, _t: int16)   = formatIntegerStr s f |> int16
    static member inline Convert (s: string, f: char, _t: uint16)  = formatIntegerStr s f |> uint16
    static member inline Convert (s: string, f: char, _t: int32)   = formatIntegerStr s f |> int32
    static member inline Convert (s: string, f: char, _t: uint32)  = formatIntegerStr s f |> uint32
    static member inline Convert (s: string, f: char, _t: int64)   = formatIntegerStr s f |> int64
    static member inline Convert (s: string, f: char, _t: uint64)  = formatIntegerStr s f |> uint64
    static member inline Convert (s: string, f: char, _t: bigint)  = formatIntegerStr s f |> bigint.Parse
    static member inline Convert (s: string, f: char, _t: float)   = float s
    static member inline Convert (s: string, f: char, _t: float32) = float32 s
    static member inline Convert (s: string, f: char, _t: decimal) = parseDecimal s

  let inline convertOne (s: string list) (f: char list) = ConvertOne.Invoke s.Head f.Head

  let inline convertOnce (s: string list) (f: char list) = ConvertOne.Invoke s.Head f.Head, s.Tail, f.Tail

  type Id<'t> (v: 't) =
    let value = v
    member __.getValue = value

  type ConvertMany =
    static member inline ConvertMany (_: 't, _: obj) = fun (s: string list, f: char list) -> convertOne s f : 't

    static member inline Invoke (s: string list) (f: char list) =
      let inline call_2 (a: ^a, b: ^b) = ((^a or ^b): (static member ConvertMany: ^b * _ -> _) b,a) (s, f)
      let inline call   (a: 'a) = call_2 (a, Unchecked.defaultof<'t>) : 't
      call Unchecked.defaultof<ConvertMany>

    static member inline ConvertMany (t: 't, _: ConvertMany) =
      fun (s, f) ->
        let _f _ = whenNestedTuple t : ('t1*'t2*'t3*'t4*'t5*'t6*'t7*'tr)
        let (t1: 't1), s, f = convertOnce s f
        let (t2: 't2), s, f = convertOnce s f
        let (t3: 't3), s, f = convertOnce s f
        let (t4: 't4), s, f = convertOnce s f
        let (t5: 't5), s, f = convertOnce s f
        let (t6: 't6), s, f = convertOnce s f
        let (t7: 't7), s, f = convertOnce s f
        let (tr: 'tr) = ConvertMany.Invoke s f
        Tuple<_,_,_,_,_,_,_,_> (t1, t2, t3, t4, t5, t6, t7, tr) |> retype : 't

    static member inline ConvertMany (_: Tuple<'t1>, _: ConvertMany) =
      fun (s: string list, f: char list) -> Tuple<_>(convertOne s f) : Tuple<'t1>

    static member inline ConvertMany (_: Id<'t1>, _: ConvertMany) =
      fun (s: string list, f: char list) -> Id<_>(convertOne s f)

    static member inline ConvertMany (_: 't1*'t2, _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, tn
    static member inline ConvertMany (_: 't1*'t2*'t3,  _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let t2, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, t2, tn
    static member inline ConvertMany (_: 't1*'t2*'t3*'t4,  _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let t2, s, f = convertOnce s f
        let t3, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, t2, t3, tn
    static member inline ConvertMany (_: 't1*'t2*'t3*'t4*'t5,  _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let t2, s, f = convertOnce s f
        let t3, s, f = convertOnce s f
        let t4, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, t2, t3, t4, tn
    static member inline ConvertMany (_: 't1*'t2*'t3*'t4*'t5*'t6,  _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let t2, s, f = convertOnce s f
        let t3, s, f = convertOnce s f
        let t4, s, f = convertOnce s f
        let t5, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, t2, t3, t4, t5, tn
    static member inline ConvertMany (_: 't1*'t2*'t3*'t4*'t5*'t6*'t7,  _: ConvertMany) =
      fun (s, f) ->
        let t1, s, f = convertOnce s f
        let t2, s, f = convertOnce s f
        let t3, s, f = convertOnce s f
        let t4, s, f = convertOnce s f
        let t5, s, f = convertOnce s f
        let t6, s, f = convertOnce s f
        let tn, _, _ = convertOnce s f
        t1, t2, t3, t4, t5, t6, tn
  
  let inline convert s f = ConvertMany.Invoke s f
