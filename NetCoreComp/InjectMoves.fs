module InjectMoves
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open AssignHomes
open FSharpx.State
open Option

type Homes = Map<Variable, Location>
type Move = 
  | StackAdj of int 
  | MovLoc of Location * Location 
  | Xchg of Location * Location
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
  | XchgA of Location * Location
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

///If someone's initial position is 
///someone else's final position, move them first.
let reorder current desired = 
  let conflicted ((l, v), (l',v')) = ( Some l = (List.tryFind (snd >> (=) v') current |> map fst))
  let conflicts, noconf = List.allPairs desired desired |> separate conflicted 
  (List.map snd conflicts @ List.map snd noconf) |> List.distinct

let rec moveManyIntoRegs' f current desired = 
  let replace evictedVar newR (l,v) = if v = evictedVar then (newR, evictedVar) else (l,v)
  match desired with
  | [] -> []
  | (r, v) :: rest
    -> let currentLoc = current |> List.tryFind (snd >> (=) v) |> map fst |> defaultWith (fun () -> f v) 
       match current |> List.tryFind (fst >> (=) r) with
       | None -> MovLoc (r, currentLoc) :: moveManyIntoRegs' f current rest
       | Some (_,x) -> Xchg (r, currentLoc) :: moveManyIntoRegs' f (List.map (replace x r) current) rest

let moveManyIntoRegs f current desired = moveManyIntoRegs' f current (reorder current desired)

let getSaveRegs homes regs = 
  homes 
  |> mapValues
  |> List.choose (function Reg x -> Some x | _ -> None)
  |> Set.ofList
  |> Set.intersect (regs |> Set.ofList)
  |> Set.toList

let toLoc (homes:Homes) = function 
  | VarAtom v -> homes.[v]
  | DataRefAtom v -> Data v
  | IntLitAtom i -> Imm i

let calleeSave = [RBP; RBX; R12; R13; R14; R15]

let initialMoves args (homes: Homes) = 
  let regArgs, stackArgs = incomingArgReqs args
  let desiredRegLocs = regArgs |> List.map ((fun (_,v) -> v, homes.[v]) >> swap)
  let currentLocs = (regArgs |> List.map (fun (r,v) -> (Reg r, v))) @ stackArgs
  let regMoves = desiredRegLocs |> moveManyIntoRegs (fun i -> failComp "only variables are args") currentLocs
  let stackMoves = stackArgs |> List.map (fun (l,v) -> MovLoc (homes.[v], l))
  regMoves @ stackMoves
//  args 
//  |> incomingArgReqs
//  |> (List.append |> uncurry)
//  |> Seq.map (fun (l,v) -> MovLoc (homes.[v], l))
  

let private callerSave = [RAX; RDI; RSI; RDX; RCX; R8; R9; R10; R11;] 

let private movToReg (homes: Homes) arith var atom = 
  let varHome = homes.[var]
  match varHome with
  | Reg r -> [],[], arith (Reg r, atom |> toLoc homes)
  | x -> [MovLoc(Reg R11, x)],[MovLoc(x, Reg R11)], arith (Reg R11, atom |> toLoc homes)
let getMoves endLab (homes: Homes) = function
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
       let regArgLocations = homes |> Map.map (fun _ i -> WithOffSet (regsToSave.Length + alignment + stackArgs.Length, i)) |> Map.toList
       let regArgMoves = regArgs 
                      |> List.map (fun (i,j) -> (Reg i, j))
                      |> moveManyIntoRegs (toLoc homes) (regArgLocations |> List.map (fun (i,j) -> (j, VarAtom i)))
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
  | Xchg (Reg r, l) -> [XchgA (Reg r, l)]
  | Xchg (l, Reg r) -> [XchgA (l, Reg r)]
  | Xchg (_, _) -> failComp "exchanging bad things";


let getAssemblyForNode (n:AssignNode) = 
  (n.beforeMoves |> List.collect toAssembly )
  @ [n.instruction] 
  @ (n.afterMoves |> List.collect toAssembly)
  

type AsmModule = {funcs : (ASTSignature *  Homes * Assembly list) list; 
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