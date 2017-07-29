module InjectMoves
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open AssignHomes
open FSharpx.State

type Move = StackAdj of int | MovLoc of Location * Location | PushM of Register | PopM of Register

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

type AssignNode = {
  id : int
  instruction : Assembly
  next : Next<int>
  beforeMoves : Move list
  afterMoves : Move list
}

let getSaveRegs homes regs = 
  homes 
  |> mapValues
  |> List.choose (function Reg x -> Some x | _ -> None)
  |> Set.ofList
  |> Set.intersect (regs |> Set.ofList)
  |> Set.toList

let toLoc (homes:Map<_,_>) = function 
  | VarAtom v -> homes.[v]
  | DataRefAtom v -> Data v
  | IntLitAtom i -> Imm i

let calleeSave = [RBP; RBX; R12; R13; R14; R15]
//todo: save caller save registers.


let initialMoves args (homes: Map<_,_>) = 
  args 
  |> incomingArgReqs
  |> (List.append |> uncurry)
  |> Seq.map (fun (l,v) -> MovLoc (homes.[v], l))
  

let private callerSave = [RAX; RDI; RSI; RDX; RCX; R8; R9; R10; R11;] 




let private movToReg (homes: Map<_,_>) arith var atom = 
  let varHome = homes.[var]
  match varHome with
  | Reg r -> [],[], arith (Reg r, atom |> toLoc homes)
  | x -> [MovLoc(Reg R11, x)],[MovLoc(x, Reg R11)], arith (Reg R11, atom |> toLoc homes)
let getMoves endLab (homes: Map<_,_>) = function
  | AddI (var,at) -> movToReg homes AddA var at
  | SubI (var,at)  -> movToReg homes SubA var at
  | IMulI (var,at)  -> movToReg homes IMulA var at
  | CmpI (var, at ) -> movToReg homes CmpA var at 
  | JmpI l -> [],[],JmpA l
  | JnzI l -> [],[],JnzA l
  | LabelI l -> [],[],LabelA l
  | AssignI (var, at) -> [],[MovLoc(homes.[var], at |> toLoc homes )],Nop 
  | ReturnI v -> [MovLoc (Reg RAX, v |> toLoc homes)],[], JmpA endLab
  | CallI (rtn, lab, args) 
    //This assumes that the normal rsp offset is even
    -> 
       let regArgs, stackArgs = args |> callingRequirements PostStack 
       let regsToSave = getSaveRegs homes callerSave

       let stackChange = stackArgs.Length + regsToSave.Length
       let alignment = ((stackChange + 1) % 2) //Calling convention requires stack alligned to 8%16 when called
       let offsetMod i x =  WithOffSet (regsToSave.Length + i + alignment,x)

       let toMovWithMod i (x, y) = MovLoc (x, y |> toLoc homes |> offsetMod i)
       let regArgMoves = regArgs |> List.map (toMovWithMod stackArgs.Length)
       let stackArgMoves = stackArgs |> List.mapi toMovWithMod

       let saveRegs = regsToSave |> List.map (fun r -> MovLoc (PostStack 0, Reg r))
       let restoreRegs = regsToSave |> List.rev |> List.map (fun r -> MovLoc (Reg r, PostStack 0))

       let saveRtnToTemp = rtn |> Option.map (fun v -> MovLoc (Reg R11, Reg RAX)) |> Option.toList
       let saveRtnToHome = rtn |> Option.map (fun v -> MovLoc (homes.[v], Reg R11)) |> Option.toList
       let adjustStack = (StackAdj (alignment + stackArgs.Length))

       let setupCall = saveRegs @ [StackAdj -alignment] @ stackArgMoves @ regArgMoves
       let afterCall = saveRtnToTemp @ [adjustStack] @ restoreRegs @ saveRtnToHome

       setupCall, afterCall, CallA lab

let toAssignNode endLab homes (c:CompNode) =
  let before, after, assem = getMoves endLab homes c.instruction
  {
    id = c.id
    instruction = assem
    next = c.next
    beforeMoves = before
    afterMoves = after
  }

let getEndLab (x:ASTSignature) = sprintf "%s_rtn" x.name |> LabelName
let computeMoves sgn il = 
  let homes = assignHomes sgn il
  let endLab = sgn |> getEndLab
  il 
  |> toGraph 
  |> fst 
  |> Map.map (toAssignNode endLab homes |> konst) ,homes

let getCallLab (x:ASTSignature) = sprintf "%s" x.name |> LabelName

let toAssembly = function 
  | StackAdj i when i > 0 -> [AddA (Reg RSP, Imm (8 * i))]
  | StackAdj i when i < 0 -> [SubA (Reg RSP, Imm (8 * -i))]
  | StackAdj i -> []
  | MovLoc (PostStack _, Reg r) -> [PushA r]
  | MovLoc (PostStack _, l) -> [ MovA (Reg R10, l); PushA R10 ]

  | MovLoc (VarStack i, VarStack j) 
     -> [MovA (Reg R10, VarStack j); 
         MovA (VarStack i, Reg R10) ]

  | MovLoc (VarStack i, PreStack j) 
     -> [MovA (Reg R10, PreStack j); 
         MovA (VarStack i, Reg R10) ]

  | MovLoc (Reg r, PostStack _)
      -> [PopA (r)]

  | MovLoc (l1,l2) -> [MovA (l1,l2)]
  | PushM r -> [PushA r]
  | PopM r -> [PopA r]


let getAssemblyForNode (n:AssignNode) = 
  (n.beforeMoves |> List.collect toAssembly )
  @ [n.instruction] 
  @ (n.afterMoves |> List.collect toAssembly)
  

type AsmModule = {funcs : (ASTSignature *  Map<Variable,Location> * Assembly list) list; 
                  lits : (string * string) list}

let assignMovesFunc sgn il = 
  let nodes, homes = computeMoves sgn il 
  let init = homes 
          |> initialMoves (List.map toVar sgn.args)
          |> Seq.collect toAssembly
          |> Seq.toList
  let body = nodes
          |> mapValues 
          |> List.collect getAssemblyForNode
  sgn, homes, init @ body
      
let assignMovesToModules (x:FlattenedModule) = {
  funcs = List.map (uncurry assignMovesFunc) x.funcs
  lits = x.lits
}
  
let saveCalleeRegs homes = getSaveRegs homes calleeSave 
                        |> List.map PushM 
                        |> List.collect toAssembly

let restoreCalleeRegs homes = getSaveRegs homes calleeSave 
                           |> List.rev 
                           |> List.map PopM 
                           |> List.collect toAssembly