module Assignment
open Flatten
open FSharpx
open FSharpx.State
open ASTBuilder
type Register = 
  | RAX
  | RDI
  | RSP
  | RSI
  | RDX
  | RCX
  | R8
  | R9

type StackPosition = {distFromBase : int; currentRspMod : int}
type Location = 
  | Reg of Register
  | Data of string
  | Stack of StackPosition 
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
  | IMulA of Location * Location
  | CallA of LabelMarker
  | PushA of Register
  | PopA of Register
  | RetA
  | SyscallA

type AssignSt = {
    ainstructs : Instruction list
    locations : Map<Variable, SavedLocation>
    stackDepth : int
    rspMod : int
    rtnLabName : string
    callLabName : string
}

let modifyRsp i = updateStateU (fun s -> {s with rspMod = s.rspMod + i})

type StateBuilder with 
  member x.Yield(i) = updateStateU (fun s -> {s with ainstructs = s.ainstructs @ [i]})
  member x.YieldFrom(xs) = mapU x.Yield xs 
let tryGetLoc a st = 
  match a with 
  | IntLitAtom i -> Imm i |> Some
  | DataRefAtom s -> Data s |> Some
  | VarAtom n -> 
      match Map.tryFind n st.locations with 
      | None -> None
      | Some (SReg r) -> Reg r |> Some
      | Some (DistFromBase pos) -> Stack {distFromBase = pos; currentRspMod = st.rspMod} |> Some

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
  return! getLoc (VarAtom var) <!> getState
}

let callingRegs = [RDI;RSI; RDX;RCX; R8; R9] 

let passArgsByConvention (SplitAt 6 (l, r)) = state {
    let! regPass = l 
                |> mapM (fun i -> getLoc i <!> getState)
               |>> Seq.zip (Reg <@> callingRegs)
               |>> Seq.map MovA
               |>> Seq.toList
    yield! regPass
    let putArgOnStack l = state {
      let! loc = getLoc l <!> getState
      yield MovA (Reg RAX, loc)
      do! modifyRsp 8
      yield PushA RAX
    }               
    do! r |> List.rev |> mapU putArgOnStack
    yield MovA (Reg RAX, Imm 0)
    return state {
      do! modifyRsp (-8 * r.Length)
      yield AddA (Reg RSP, ( Imm (8*r.Length)))
      }
  }

let saveRegisters = state {
  let callerSaveRegs = [RAX] 
  let rspMod = (List.length callerSaveRegs * 8)
  do! modifyRsp rspMod
  for r in callerSaveRegs do yield PushA (r)
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
  | i -> return failf "rsp is somehow unaligned to %i. Oh dear god" i
}

///Predlude for calling functions. Returns the coressponding epilogue
let callPrologue args = state {
  let! restoreRegisters = saveRegisters 
  let! undoArgPass = passArgsByConvention args
//  let! restoreRsp = //alignRsp
  return state {
//    do! restoreRsp
    do! undoArgPass
    do! restoreRegisters
  }
}


let assignInstruct = 
  let handleArithWithRax instr a b = state {
      let! s = getState
      let aloc = getLoc (VarAtom a) s 
      yield MovA (Reg RAX, aloc)
      let bloc = getLoc b s
      yield instr (Reg RAX, bloc)
      yield MovA (aloc, Reg RAX) }

  function
  | CmpI (a,b) -> state {
      let! l1 = (getLoc (VarAtom a) <!> getState) 
      let! l2 = (getLoc b <!> getState)
      yield CmpA (l1, l2) }
  | AssignI (s,v) -> state {
      let! st = getState
      let valLoc = getLoc v st
      yield MovA (Reg RAX, valLoc)
      let! setLoc = match tryGetLoc (VarAtom s) st with
                    | Some s -> s |> returnM
                    | None -> assignLocOnStack s 
      yield MovA (setLoc, Reg RAX) }
  | JnzI l -> state { yield JnzA l }
  | JmpI l -> state { yield JmpA l }
  | ReturnI rtnVal -> state { 
      let! label = (fun i-> i.rtnLabName) <!> getState
      let! rtnLoc = getLoc rtnVal <!> getState
      yield MovA (Reg RAX, rtnLoc)
      yield JmpA (label |> LabelName)
   }
  | LabelI l -> state { yield LabelA l }
  | AddI (a,b) -> handleArithWithRax AddA a b
  | SubI (a,b) -> handleArithWithRax SubA a b
  | IMulI (a,b) -> handleArithWithRax IMulA a b
  | CallI (v, l, args) -> state {
      let! callEpilogue = callPrologue args
      yield CallA l 
      let! rtnLoc = assignLocOnStack v
      yield MovA (rtnLoc, Reg RAX)
      do! callEpilogue }

let handleArgs (SplitAt 6 (l,r)) = state {
  let! loc = mapM (fun (i : ASTVariable) -> assignLocOnStack (VarName i.name)) l
  yield! loc 
      |> fun i -> Seq.zip i (Reg <@> callingRegs)
      |> List.ofSeq
      |> List.map MovA

  do! r
   |> List.indexed
   |> mapU (fun (i,v) -> assignLoc (VarName v.name) (DistFromBase (-8 * (i+1))))
}


let assign (s: ASTSignature, xs) = 
  let work = handleArgs s.args *> mapU assignInstruct xs
  let init = {
    stackDepth = 0
    rspMod = 0
    ainstructs = []
    locations = Map.empty
    rtnLabName = s.name + "_rtn"
    callLabName = s.name
  }
  (s, exec work init)

type AssignedModule = {funcInstructions : (ASTSignature * AssignSt) list; dataLits : (string * string) list }
let assignModule {funcs = xss; lits = l} = {funcInstructions = List.map assign xss; dataLits = l}








