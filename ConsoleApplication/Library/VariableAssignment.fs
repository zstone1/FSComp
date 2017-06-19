module VariableAssignment
open Parser
open ASTBuilder
open FSharpx.State 
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
type Location = Reg of Register | Mem of Memory | Stack

type AssignedVar = AVar of Ty * string * Location

type VarState = {
  variables : AssignedVar list
  functions : ASTFuncRef  list
}
let vars x = x.variables
let funcs x = x.functions

type VarStateM<'t> = State<'t,VarState>

let getAssignedRegisters st = 
  List.choose 
    (function | (AVar (_,_,Reg x))  -> Some x | _ -> None)
    st.variables


//based on calling convention for C functions
//let initializeScope args = 















