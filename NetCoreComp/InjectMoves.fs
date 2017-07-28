module InjectMoves
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open AssignHomes
open FSharpx.State

type Move = StackAdj of int | MovLoc of Location * Location

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

type AssignNode = {
  id : int
  instruction : Assembly
  next : Next<int>
  beforeMoves : Move list
  afterMoves : Move list
}


let toLoc (homes:Map<_,_>) = function 
  | VarAtom v -> homes.[VarAtom v]
  | DataRefAtom v -> Data v
  | IntLitAtom i -> Imm i

let initialMoves args (homes: Map<_,_>) = 
  args 
  |> List.map VarAtom
  |> incomingArgReqs
  |> Seq.map (fun (l,v) -> MovLoc (l, homes.[v]))

let private movToReg (homes: Map<_,_>) arith var atom = 
  let varHome = homes.[VarAtom var]
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
  | AssignI (var, at) -> [],[], MovA (homes.[VarAtom var], at |> toLoc homes )
  | ReturnI v -> [MovLoc (Reg RAX, homes.[v])],[], JmpA endLab
  | CallI (rtn, lab, args) 
    -> let reqs = args |> callingRequirements PostStack |> Seq.map (fun (x,y) -> MovLoc (x,homes.[y])) |> List.ofSeq
       let stackArgCount = max (Seq.length reqs - 6) 0
       let offsetStack = ((stackArgCount + 1) % 2)
       let saveRtnToTemp = rtn |> Option.map (fun v -> MovLoc (Reg R11, Reg RAX)) |> Option.toList
       let saveRtnToHome = rtn |> Option.map (fun v -> MovLoc (homes.[VarAtom v], Reg R11)) |> Option.toList
       let adjustStack = (StackAdj -(offsetStack + stackArgCount))
       (StackAdj offsetStack) :: reqs , saveRtnToTemp @ [adjustStack] @ saveRtnToHome, CallA lab

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
  let homes = assignHomes il
  let endLab = sgn |> getEndLab
  il 
  |> toGraph 
  |> fst 
  |> Map.map (toAssignNode endLab homes |> konst) 

let getCallLab (x:ASTSignature) = sprintf "%s_call" x.name |> LabelName

let toAssembly = function 
  | StackAdj i when i > 0 -> [AddA (Reg RSP, Imm i)]
  | StackAdj i when i < 0 -> [SubA (Reg RSP, Imm i)]
  | StackAdj i -> []
  | MovLoc (PostStack _, Reg r) -> [PushA r]
  | MovLoc (PostStack _, l) -> [ MovA (Reg R10, l); PushA R10 ]
  | MovLoc (l1, l2) -> [ MovA (l1,l2) ]

let getAssemblyForNode (n:AssignNode) = 
  (n.beforeMoves |> List.collect toAssembly )
  @ [n.instruction] 
  @ (n.afterMoves |> List.collect toAssembly)
  
