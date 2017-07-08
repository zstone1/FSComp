[<AutoOpen>]
module CoreTypes

open FSharpx.State

exception CompilerError of string with 
  override x.ToString() = sprintf "Failed to compile %A" x.Data0


let public failComp s = raise (CompilerError s)
let public failf a = Printf.kprintf failComp a

let (|SplitAt|) i = function 
  | xs when List.length xs < i -> (xs,[])
  | xs -> (List.take 6 xs, List.skip 6 xs)

let (<@>) f l = List.map f l

[<Literal>]
let PlusName = "_+"
[<Literal>]
let MinusName = "_-"

[<Literal>]
let MultName = "_*"
