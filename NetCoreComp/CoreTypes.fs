[<AutoOpen>]
module CoreTypes

open FSharpx.State

exception CompilerError of string with 
  override x.ToString() = sprintf "Failed to compile %A" x.Data0


let public failComp s = raise (CompilerError s)
let public failf a = Printf.kprintf failComp a

let (|SplitAt|) i = function 
  | xs when List.length xs < i -> (xs,[])
  | xs -> (List.take i xs, List.skip i xs)


[<Literal>] 
let PlusName = "_+"
[<Literal>]
let MinusName = "_-"

[<Literal>]
let MultName = "_*"
