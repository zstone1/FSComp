module Assignment
open Flatten
open FSharpx
open FSharpx.State
type Register = 
  | RAX
  | RDI
  | RSI

type Location = 
  | Reg of Register
  | Stack of int
  | Imm of int


type AInstruct = 
  | CmpA of Location * Location
  | MovA of Location * Location
  | JNZA of LabelMarker
  | RetA
  | LabelA of LabelMarker
  | AddA of Location * Location
  | SubA of Location * Location

type AssignSt = {
    ainstructs : AInstruct list
    locations : Map<string, Location>
    stackDepth : int
}
let addInstruct i = updateState' (fun s -> {s with ainstructs = s.ainstructs @ [i]})
let Cmp a1 a2 = CmpA (a1,a2) |> addInstruct
let Mov a1 a2 = MovA (a1, a2) |> addInstruct
let Jnz l = JNZA l |> addInstruct
let Ret = RetA |> addInstruct
let Label l = LabelA l |> addInstruct
let Add a1 a2 = AddA (a1,a2) |> addInstruct
let Sub a1 a2 = SubA (a1,a2) |> addInstruct

let tryGetLoc a st = 
  match a with 
  | IntLitAtom i -> Imm i |> Some
  | VarName n -> Map.tryFind n st.locations

let getLoc a st = 
  match tryGetLoc a st with
  | Some s -> s
  | None -> failf "cannot find variable %A" a

let assignLoc loc var = 
  let updater s = {s with locations = Map.add var loc s.locations}
  updateState' updater

let getNextLoc = state{
    let! s = getState
    let nextStackLoc = Stack s.stackDepth
    do! putState {s with stackDepth = s.stackDepth + 8}
    return nextStackLoc
}

let assign = function
  | CmpI (a,b) -> state {
      let! l1 = (getLoc a <!> getState) 
      let! l2 = (getLoc b <!> getState)
      do! Cmp l1 l2 }
  | AssignI (s,v) -> state{
      let! st = getState
      let valLoc = getLoc v st
      let! setLoc = match tryGetLoc (VarName s) st with
                    | Some s -> s |> returnM
                    | None -> getNextLoc
      do! Mov setLoc valLoc }














