[<AutoOpen>]
module CoreTypes
open FSharpx.State

exception CompilerError of string

let public failComp s = raise (CompilerError s)

type CompilerState = {
  uniqueCounter : int 
}

type Comp<'t> = State<'t, CompilerState>

let comp = new StateBuilder()

let nextName : Comp<string> = comp {
  let! {uniqueCounter = i} as s = getState
  do! putState {s with uniqueCounter = i+1}
  return sprintf ".%i" i }



