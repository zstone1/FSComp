module InjectMoves
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open AssignHomes
open FSharpx.State
open Option
open MixedLang
open Unification

type Offset = 
  | FromHome of int

type Move = 
  | StackAdj of int 
  | MovLoc of Location * Location
  | PushM of Register 
  | PopM of Register

type Assembly = 
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
  | Nop

let getSaveRegs used regs = 
  used 
  |> List.choose (function Reg x -> Some x | _ -> None)
  |> Set.ofList
  |> Set.intersect (regs |> Set.ofList)
  |> Set.toList


let calleeSave = [RBP; RBX; R12; R13; R14; R15]

let private callerSave = [RDI; RSI; RDX; RCX; R8; R9; R10; R11;] 

let fromAtom = function 
  | VarAtom v -> v
  | DataRefAtom s -> Data s
  | IntLitAtom i -> Imm i

let private movToReg arith var atom = 
  match var with
  | Reg r -> [], [arith (Reg r, atom |> fromAtom )], []
  | x -> let before = [MovLoc(Reg R11, x)]
         let after = [MovLoc(x , Reg R11)]
         let work =  [arith (Reg R11, atom |> fromAtom)]
         before, work, after

let getMoves usedLocs endLab = function
  | AddI (var,at) -> movToReg AddA var at
  | SubI (var,at)  -> movToReg SubA var at
  | IMulI (var,at)  -> movToReg IMulA var at
  | CmpI (var, at) -> movToReg CmpA var at
  | JmpI l -> [], [JmpA l], []
  | JnzI l -> [], [JnzA l], []
  | LabelI l -> [], [LabelA l], []
  | AssignI (var, at) -> [], [], [MovLoc(var, at |> fromAtom)] 
  | ReturnI v -> [], [JmpA endLab], []

  //Ok, this is where it gets weird. We assume the ML puts rtn in RAX correctly
  //we also know that the args are handled by previous assignments, and the PrepareCall/CompleteCall wrappers
  | CallI (rtn, lab, args) -> [], [CallA lab], []
  | PrepareCall i 
    -> let requireSaving = callerSave |> getSaveRegs usedLocs 
       let offset = if ((requireSaving.Length + i) % 2) = 0 then [StackAdj -1] else []
       let saves = requireSaving |> List.map PushA
       offset, saves, []
  | CompleteCall i 
    -> let requireSaving = callerSave |> getSaveRegs usedLocs 
       let offset = if ((requireSaving.Length + i) % 2) = 0 then [StackAdj 1] else []
       let saves = requireSaving |> List.map PopA |> List.rev
       [StackAdj (i)], saves, offset

let toAssignNode endLab usedLocs (c:CompNode<_>) = getMoves usedLocs endLab c.instruction

let getEndLab (x:UnifiedSignature) = sprintf "%s_rtn" x.name |> LabelName

let computeMoves sgn ml = 
  let usedLocs = ml |> List.collect getReadVariables
  let endLab = sgn |> getEndLab
  ml |> List.map (getMoves usedLocs endLab )

let getCallLab (x:UnifiedSignature) = sprintf "%s" x.name |> LabelName

let toAssembly = function 
  | StackAdj i when i > 0 -> [AddA (Reg RSP, Imm (8 * i))]
  | StackAdj i when i < 0 -> [SubA (Reg RSP, Imm (8 * -i))]
  | StackAdj i -> []
  | MovLoc (Stack (PostStack _), Reg r) -> [PushA r]
  | MovLoc (Stack (PostStack _), l) -> [ MovA (Reg R10, l); PushA R10 ]

  | MovLoc (Stack (a), Stack(c)) 
     -> [MovA (Reg R10, Stack(c)) 
         MovA (Stack(a), Reg R10) ]

  | MovLoc (Reg r, Stack (PostStack _))
      -> [PopA (r)]

  | MovLoc (l1,l2) -> [MovA (l1,l2)]
  | PushM r -> [PushA r]
  | PopM r -> [PopA r]


let getAssemblyForNode (before, work, after) = 
  (before |> List.collect toAssembly )
  @ work
  @ (after |> List.collect toAssembly)
  

type AsmModule = {funcs : (Assembly list) list; 
                  lits : (string * string) list}

let saveCalleeRegs homes = getSaveRegs homes calleeSave 
                        |> List.map PushM 
                        |> List.collect toAssembly
let stackHomesOffset homes = homes 
                          |> List.map (function Stack (VarStack i) -> i | _ -> 0) 
                          |> List.max
                          |> ((*) 8)

let restoreCalleeRegs homes = getSaveRegs homes calleeSave 
                           |> List.rev 
                           |> List.map PopM 
                           |> List.collect toAssembly

let assignMovesFunc (sgn, ml) = 
  let savedVariables = (ml |> List.collect getReadVariables)
  let body = computeMoves sgn ml |> List.collect getAssemblyForNode
  let init = LabelA (getCallLab sgn) 
          :: SubA (Reg RSP, stackHomesOffset savedVariables |> Imm)
          :: saveCalleeRegs savedVariables
  let finish = LabelA (getEndLab sgn) 
            :: restoreCalleeRegs savedVariables
             @ [AddA (Reg RSP, stackHomesOffset savedVariables |> Imm)]
             @ [RetA]
  init @ body @ finish
      
let assignMovesToModules (x:UnifiedModule) = 
  {
    funcs = List.map assignMovesFunc x.funcs
    lits = x.lits
  }
  
