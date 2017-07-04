[<AutoOpen>]
module CoreTypes

open FSharpx.State

exception CompilerError of string with 
  override x.ToString() = sprintf "Failed to compile %A" x.Data0


let public failComp s = raise (CompilerError s)
let public failf a = Printf.kprintf failComp a

[<Literal>]
let PlusName = "_+"
[<Literal>]
let MinusName = "_-"

