module VariableAssignment

open Parser
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
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

let AllRegisters = [RAX; RCX; RDX; RBX; RSP; RBP; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15]
  
type ScaleFactors = One|Two|Four|Eight
type Scale = Register * ScaleFactors
type Memory =
  | ByNum of int
  | ByReg of Register
  | ByReg_Scale of Register * Scale
  | ByReg_Num of Register * int
  | ByReg_Scale_Num of Register * Scale * int

type Location =
  | Reg of Register 
//  | Mem of Memory 
//  | OnStack of int 
  | RedZone of int
  | Imm of int


type AssignedVar<'T> = AVar of 'T * Location

let getLocation (AVar (_,l)) = l

type VarState = { 
  variables : Map<string,AssignedVar<ASTVariable>>
  functions : ASTFuncRef list
  redZone : int
}
let getVariables s = s.variables

let setVars f st : VarState = {st with variables = f st.variables}

type VarStateM<'t> = State<'t,VarState>

let incrRedZone = state {
  let incr s = {s with redZone = s.redZone + 8}
  let! {redZone = r} = updateState incr
  return r
}

let addVariable var l = state {
  let! s = getState
  let v = AVar (var,l)
  let newState = Map.add (name var) v s.variables
  do! putState {s with variables = newState}
}

let addLocal var = state {
  let! offset = incrRedZone
  do! addVariable var (RedZone offset)
}

let getVar n = getState 
           |>> getVariables
           |>> Map.tryFind n
           |>> function | Some x -> x
                        | None -> failf "failed to find variable %s in var state, which is bad" n

let callingConvention = List.map Reg [RDI;RSI;RDX;RCX;R8;R9] 
  
//based on calling convention for C functions
let initScope (a:ASTVariable list) = 
  let vars = a
          |> flip Seq.zip callingConvention 
          |> Seq.map (fun i -> (i |> fst |> name, AVar i))
  {
    functions = []
    redZone = 0
    variables = Map.ofSeq vars
  }

let rec getExprLocation = function 
  | IntLit i -> Imm i |> returnM
  | Variable v -> getVar v |>> getLocation
  | Func ("Add",[_;_]) | Func ("Sub",[_;_]) -> Reg RAX |> returnM

  | Func _ -> failf "Only add and sub are supported right now" 
  | StringLit _ -> failf "I'll deal with string constants later"

let assignStatement = function
  | ReturnStat _ | Assignment _ | Execution _ | IfStat _ 
    as stmt -> stmt |> returnM
  | Declaration v as stmt-> addLocal v >>. returnM stmt

let assignStatement' s = state {
  let! state = getState
  let! expr = assignStatement s
  return (expr, state)
}

let convertFunc f =
  let initial = initScope f.signature.args
  let assigned = mapM assignStatement' f.body 
  eval assigned initial

 











