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

type SavedLocation =
  | SReg of Register
  | DistFromBase of int


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
    locations : Map<string, SavedLocation>
    stackDepth : int
    rspMod : int
    rtnLabName : string
    callLabName : string
}

let modifyRsp i = updateStateU (fun s -> {s with rspMod = s.rspMod + i})

let addInstruct i = updateStateU (fun s -> {s with ainstructs = s.ainstructs @ [i]})

type StateBuilder with
  member x.Yield(i) = i |> addInstruct

let tryGetLoc a st = 
  match a with 
  | IntLitAtom i -> Imm i |> Some
  | VarName n -> 
      match Map.tryFind n st.locations with 
      | None -> None
      | Some (SReg r) -> Reg r |> Some
      | Some (DistFromBase pos) -> Stack (pos, st.rspMod) |> Some

let getLoc a st = 
  match tryGetLoc a st with
  | Some s -> s
  | None -> failf "cannot find variable %A" a

let assignLoc var loc = 
  let updater s = {s with locations = Map.add var loc s.locations}
  updateStateU updater

let incrementStackDepth = state{
    let! s = getState
    let nextStackLoc = s.stackDepth + 8
    do! putState {s with stackDepth = s.stackDepth + 8}
    return nextStackLoc
}

let assignLocOnStack var = state {
  let! pos = incrementStackDepth
  let loc = DistFromBase pos
  do! assignLoc var loc
  return! getLoc (VarName var) <!> getState
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
  let callerSaveRegs = [RAX; RDI]
  let rspMod = (List.length callerSaveRegs * 8)
  do! modifyRsp rspMod
  for r in callerSaveRegs do yield PushA r
  return (state {
    for r in List.rev callerSaveRegs do yield PopA r
    do! modifyRsp (-1 * rspMod)})
}

let alignRsp = state {
  let! modifier = (fun i -> i.rspMod) <!> getState
  match modifier % 16 with 
  | 8 -> return empty
  | 0 -> do! modifyRsp 8
         yield SubA (Reg RSP, Imm 8)
         return (state {
           yield AddA (Reg RSP, Imm 8)
           do! modifyRsp -8
         })
  | i -> return failf "rsp is somehow aligned to %i. Oh dear god" i
}

///Predlude for calling functions. Returns the coressponding epilogue
let callPrologue = state {
  let! restoreRegisters = saveRegisters
  let! restoreRsp = alignRsp
  return state {
    do! restoreRsp
    do! restoreRegisters
  }
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








