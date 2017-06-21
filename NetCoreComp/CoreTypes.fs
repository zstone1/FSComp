[<AutoOpen>]
module CoreTypes

open FSharpx.State

exception CompilerErrorException of string

let public failComp s = raise (CompilerErrorException s)
let public failf a = Printf.kprintf failComp a

type CompilerState = {
  uniqueCounter : int 
}

type Comp<'t> = State<'t, CompilerState>

let comp = StateBuilder()

let nextName : Comp<string> = comp {
  let! {uniqueCounter = i} as s = getState
  do! putState {s with uniqueCounter = i+1}
  return sprintf ".%i" i }

let public updateState (f:'State -> 'State) : State<'State,'State> = state{
  let! prev = getState
  let next = f prev
  do! putState next
  return next
}

let ignoreM s = map ignore s

let public trd (_,_,c) = c

let inline public (|>>) m f = f <!> m

let flip3' f a b c = f a c b