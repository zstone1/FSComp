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
  | Stack of int
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
    let nextStackLoc = s.stackDepth
    do! putState {s with stackDepth = s.stackDepth + 8}
    return nextStackLoc
}

let handleArithWithRax instr a b = state {
      let! s = getState
      let aloc = getLoc a s 
      yield MovA (Reg RAX, Reg RAX)
      yield MovA (Reg RAX, aloc)
      let bloc = getLoc b s
      yield instr (Reg RAX, bloc)
      yield MovA (aloc, Reg RAX) }

/// calling convention requires stack point to be a 
/// an 8%16 position before executing a call. 
/// Returns the work required to undo this.
let ensureRspAt8BitRegister = state {
  let! s = getState 
  match s.stackDepth % 16 with
  | 0 -> yield SubA (Reg RSP, Imm 8)
         return state { yield AddA (Reg RSP, Imm 8) }
  | 8 -> return empty
  | _ -> return failf "oh god registers are horribly unaligned"
} 

let saveRegisters = state {
  yield PushA (RAX)
  return (state {yield PopA RAX})
}

///Predlude for calling functions. Returns the coressponding epilogue
let callPrologue = state {
  let! restoreRegisters = saveRegisters
  let! undoStackPad = ensureRspAt8BitRegister
  return state {
    do! undoStackPad
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
                    | None -> state { let! l = incrementStackDepth
                                      do! assignLoc s (Stack l)
                                      return (Stack l)}
      yield MovA (setLoc, Reg RAX) }
  | JNZI l -> state { yield JnzA l }
  | JmpI l -> state { yield JmpA l }
  | ReturnI s -> state { 
      let! label = (fun i-> i.rtnLabName) <!> getState
      let! rtnLoc = getLoc s <!> getState
      yield MovA (Reg RAX, rtnLoc)
      yield JmpA (label |> LabelName)
   }

//  | ReturnI s -> state {
//      let! loc =  getLoc s <!> getState
//      yield MovA (Reg RDI, loc)
//      yield MovA (Reg RAX, Imm 60)
//      yield SyscallA }
  | LabelI l -> state { yield LabelA l }
  | AddI (a,b) -> handleArithWithRax AddA a b
  | SubI (a,b) -> handleArithWithRax SubA a b
  | CallI (rtn, l, []) -> state {
    let! callEpilogue = callPrologue
    yield CallA l 
    do! callEpilogue }
  | CallI _ -> failf "no args yet"

let assign (s: ASTSignature, xs) = 
  let work = mapMUnit assignInstruct xs
  let init = {
    stackDepth = 0
    ainstructs = []
    locations = Map.empty
    rtnLabName = s.name + "_rtn"
    callLabName = s.name
  }
  (s, exec work init)

let assignModule xss = List.map assign xss








