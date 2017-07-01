module Assignment
open Flatten
open FSharpx
open FSharpx.State
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
  | LabelA of LabelMarker
  | AddA of Location * Location
  | SubA of Location * Location
  | SyscallA

type AssignSt = {
    ainstructs : Instruction list
    locations : Map<string, Location>
    stackDepth : int
}
let addInstruct i = updateState' (fun s -> {s with ainstructs = s.ainstructs @ [i]})
let Cmp a1 a2 = CmpA (a1,a2) |> addInstruct
let Mov a1 a2 = MovA (a1, a2) |> addInstruct
let Jnz l = JnzA l |> addInstruct
let Label l = LabelA l |> addInstruct
let Add a1 a2 = AddA (a1,a2) |> addInstruct
let Sub a1 a2 = SubA (a1,a2) |> addInstruct
let Syscall = SyscallA |> addInstruct

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

let getNextLoc = state{
    let! s = getState
    let nextStackLoc = Stack s.stackDepth
    do! putState {s with stackDepth = s.stackDepth + 8}
    return nextStackLoc
}

let handleArithWithRax instr a b = state {
      let! s = getState
      let aloc = getLoc a s 
      do! Mov (Reg RAX) aloc
      let bloc = getLoc b s
      do! instr (Reg RAX) bloc
      do! Mov aloc (Reg RAX) }

let assignInstruct = function
  | CmpI (a,b) -> state {
      let! l1 = (getLoc a <!> getState) 
      let! l2 = (getLoc b <!> getState)
      do! Cmp l1 l2 }
  | AssignI (s,v) -> state {
      let! st = getState
      let valLoc = getLoc v st
      do! Mov (Reg RAX) valLoc
      let! setLoc = match tryGetLoc (VarName s) st with
                    | Some s -> s |> returnM
                    | None -> state { let! l = getNextLoc; 
                                      do! assignLoc s l
                                      return l}
      do! Mov setLoc (Reg RAX) }
  | JNZI l -> Jnz l
  | ReturnI s -> state {
      let! loc =  getLoc s <!> getState
      do! Mov (Reg RDI) loc
      do! Mov (Reg RAX) (Imm 60)
      do! Syscall }
  | LabelI l -> Label l
  | AddI (a,b) -> handleArithWithRax Add a b
  | SubI (a,b) -> handleArithWithRax Sub a b

let assign xs = 
  let work = mapMUnit assignInstruct xs
  let init = {
    stackDepth = 0
    ainstructs = []
    locations = Map.empty
  }
  exec work init

let assignModule xss = List.map assign xss








