module AssignHomes
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open FSharpx.State
(*
  The stack is broken up into three parts
  The PreStack, VarStack, and PostStack. 
  Basically, PreStack is for incoming args, VarStack
  is for homeless variables, and PostStack is for 
  outgoing args. That is

  int foo(int x, int y){
    int a = 2;
    int b = 2;
    bar(a,b);
    return 1;
  }
  (assuming y didn't live in a register, and 
   calling convention had all args on stacks.)
(index : value)
  ---PreStack
2 : y
1 : x 
  instrptr (managed by call/ret)
  ---VarStack
1 : a 
2 : b
  ---PostStack
2 : b
1 : a
  ---

A key assumption here is the PostStack is transient,
and the PreStack and VarStack is fixed. The PostStack
persists only for one function call

*)
type Register = 
  | RAX
  | RCX
  | RDX
  | RBX
  | RSP
  | RBP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15


type Location = 
  | Reg of Register
  | Data of string
  | PreStack of int
  | Imm of int
  | PostStack of int
  | WithOffSet of int * Location
  | VarStack of int

 
let callingRegs = [RDI;RSI;RDX;RCX;R8;R9] 
//The registers available to be homes for variables. 
//What's missing is RSP (duh), and R10,R11. I am 
//reserving these for swaping, temp storage, ect. 
//Also the calling regs are missing to avoid conflicts
//while handling parameters
let homeRegisters = callingRegs @ [R15; R14; R13; R12; RBP; RBX; RAX; ]
let callingRequirements stackPos (SplitAt 6 (l,r)) = 
  let regArgs = Seq.zip callingRegs l |> Seq.toList
  let stackArgs = r
               |> List.rev
               |> List.indexed
               |> List.map (fun (i,j) -> (i |> stackPos, j))
  (regArgs, stackArgs)

let incomingArgReqs l = callingRequirements (PreStack) l

let getInstrAffinity = function
  | CallI (v, _, l)
    -> let regArgs, stackArgs = callingRequirements (PostStack) l 
       let rtn = v |> Option.map (fun i -> (Reg RAX, VarAtom i)) |> Option.toList
       rtn @ (List.map (fun (i,j) -> (Reg i, j)) regArgs) @ stackArgs
       
  | ReturnI v -> [Reg RAX, v] 
  | _ -> []

let private getAfinity (compNode:CompNode) = (getInstrAffinity compNode.instruction) 

let private allAffinities il = 
  il 
  |> (toGraph >> fst)
  |> Map.fold (fun s _ v -> Seq.append s (getAfinity v)) Seq.empty
  |> Seq.fold (fun s (l,a) -> Map.add a l s) Map.empty

let allVariables sgn il = 
  let getAllVariables v = getReadVariables v.instruction @ getWrittenVariables v.instruction
  let inprgm = il 
            |> (toGraph >> fst)
            |> Map.fold (fun s _ v -> List.append (getAllVariables v) s) List.empty
  let args = List.map toVar sgn.args
  inprgm @ args
  
let assignHomesStackOnly sgn il = 
  let allVars = il |> allVariables sgn |> List.distinct 
  let homes = (Seq.initInfinite VarStack)// |> Seq.append (List.map Reg homeRegisters) 
//  let affinities = il |> allAffinities
//Hack to just use stack space for everything.
  Seq.zip allVars homes |> Map.ofSeq

let assignHomesRegGreedy sgn il = 
  let allVars = il |> allVariables sgn |> List.distinct 
  let homes = (Seq.initInfinite VarStack) |> Seq.append (List.map Reg homeRegisters) 
  Seq.zip allVars homes |> Map.ofSeq

let assignHomes sgn il = 
  match globalSettings.allocation with 
  | StackOnly -> assignHomesStackOnly sgn il
  | RegGreedy -> assignHomesRegGreedy sgn il