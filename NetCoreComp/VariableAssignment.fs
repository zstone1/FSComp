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
  | Mem of Memory 
  | OnStack of int 
  | Imm of int


type AssignedVar<'T> = AVar of 'T * Location

type VarState = { 
  variables : Map<string,AssignedVar<ASTVariable>>
  functions : ASTFuncRef
  stack : int
}

let setVars f st : VarState = {st with variables = f st.variables}

let stack f st = {st with stack = f st}


type VarStateM<'t> = State<'t,VarState>

(*
let getAssignedRegisters st = 
     st.variables 
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.choose ( function | AVar (_, Reg x)  -> Some x | _ -> None) 

let verifyLocationAvailable = function
  | Reg r -> state {
    let! assigned = getAssignedRegisters <!> getState
    return if Seq.contains r assigned
      then failf "Register %A is not available" r
      else () }
  | OnStack i -> 
*)


(*
  //{st with variables = Map.add name v st.variables}

let pushStack v = state {
  let! st = getState
  let newStackDepth= st.stack + 1
  do! putState {st with stack = newStackDepth}
  
  match avar with 
  | (AVar (v, Reg r)) -> updateState (fun st -> {st with stack = st.stack + 1})
  | x -> failf "Variable %A is not in a register, so cannot be pushed to stack" x



}


let popStack reg = state {
  let! vs = stack <!> getState
  match vs with
  | [] -> return failf "Cannot pop an empty stack."
  | v::vs -> do! updateState' (fun st -> {st with stack = vs})
             do 

}


let assign loc var = AVar (var,loc) 
                  |> addVariable 
                  |> updateState 
                  
let push v = state {
  do! updateState (fun st -> ) |>> ignore
  return AVar (v, OnStack)}


let pop r = state {
  let! {stack = s} as st = getState;
  match s with
  | v::vs -> do! putState { st with stack = vs} 
             return v
  | [] -> return failf "Cannot pop empty stack!"
} 

let callingConvention = seq {
  let regs = [RDI;RSI;RDX;RCX;R8;R9]
  let ops = List.map (fun r a -> AVar (a, Reg r) ) regs
  yield! Seq.initInfinite (konst OnStack)
}
  
//based on calling convention for C functions
let initScope a = a
               |> flip Seq.zip callingConvention
               |> Seq.map AVar
               |> Seq.toList
               |> addVariables
               |> updateState

let setupCall ()
 
*)












