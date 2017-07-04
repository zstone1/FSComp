module Assignment
open Flatten
open FSharpx
open FSharpx.State
open ASTBuilder
type Register = 
  | RAX
  | RDI
  | RSP

type Location = 
  | Reg of Register
  | Stack of int (*base dist*) * int (*rspMod*)
  | Imm of int


type Instruction = 
  | CmpA of Location * Location
  | MovA of Location * Location
  | JnzA of LabelMarker
  | JmpA of LabelMarker
  | LabelA of LabelMarker
  | AddA of Location * Location
  | SubA of Location * Location
  | CallA of LabelMarker
  | PushA of Register
  | PopA of Register
  | RetA
  | SyscallA

type AssignSt = {
    ainstructs : Instruction list
    locations : Map<string, Location>
    stackDepth : int
    rspMod : int
    rtnLabName : string
    callLabName : string
}

let addInstruct i = updateState' (fun s -> {s with ainstructs = s.ainstructs @ [i]})

type StateBuilder with
  member x.Yield(i) = i |> addInstruct

let tryGetLoc a st = 
  match a with 
  | IntLitAtom i -> Imm i |> Some
  | VarName n -> Map.tryFind n st.locations

let getLoc a st = 
  match tryGetLoc a st with
  | Some s -> s
  | None -> failf "cannot find variable %A" a

let assignLoc var loc = 
  let updater s = {s with locations = Map.add var loc s.locations}
  updateState' updater

let incrementStackDepth = state{
    let! s = getState
    let nextStackLoc = s.stackDepth + 8
    do! putState {s with stackDepth = s.stackDepth + 8}
    return nextStackLoc
}

let assignLocOnStack var = state {
  let! pos = incrementStackDepth
  let! modifier = (fun i -> i.rspMod) <!> getState
  let loc = Stack (pos, modifier)
  do! assignLoc var loc
  return loc
}

let handleArithWithRax instr a b = state {
      let! s = getState
      let aloc = getLoc a s 
      yield MovA (Reg RAX, Reg RAX)
      yield MovA (Reg RAX, aloc)
      let bloc = getLoc b s
      yield instr (Reg RAX, bloc)
      yield MovA (aloc, Reg RAX) }

let saveRegisters = state {
  yield PushA (RAX)
  return (state {yield PopA RAX})
}

///Predlude for calling functions. Returns the coressponding epilogue
let callPrologue = state {
  //let! restoreRegisters = saveRegisters
  return empty
}

let assignInstruct = function
  | CmpI (a,b) -> state {
      let! l1 = (getLoc a <!> getState) 
      let! l2 = (getLoc b <!> getState)
      yield CmpA (l1, l2) }
  | AssignI (s,v) -> state {
      let! st = getState
      let valLoc = getLoc v st
      yield MovA (Reg RAX, valLoc)
      let! setLoc = match tryGetLoc (VarName s) st with
                    | Some s -> s |> returnM
                    | None -> assignLocOnStack s 
      yield MovA (setLoc, Reg RAX) }
  | JNZI l -> state { yield JnzA l }
  | JmpI l -> state { yield JmpA l }
  | ReturnI s -> state { 
      let! label = (fun i-> i.rtnLabName) <!> getState
      let! rtnLoc = getLoc s <!> getState
      yield MovA (Reg RAX, rtnLoc)
      yield JmpA (label |> LabelName)
   }
  | LabelI l -> state { yield LabelA l }
  | AddI (a,b) -> handleArithWithRax AddA a b
  | SubI (a,b) -> handleArithWithRax SubA a b
  | CallI ((VarName n), l, []) -> state {
    let! callEpilogue = callPrologue
    yield CallA l 
    let! rtnLoc = assignLocOnStack n
    yield MovA (rtnLoc, Reg RAX)
    do! callEpilogue }
  | CallI _ -> failf "no args yet"

let assign (s: ASTSignature, xs) = 
  let work = mapMUnit assignInstruct xs
  let init = {
    stackDepth = 0
    rspMod = 0
    ainstructs = []
    locations = Map.empty
    rtnLabName = s.name + "_rtn"
    callLabName = s.name
  }
  (s, exec work init)

let assignModule xss = List.map assign xss








