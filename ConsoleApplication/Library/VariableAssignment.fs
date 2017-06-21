module VariableAssignment
open Parser
open ASTBuilder
open FSharpx.State 
open FSharpx.Functional
 
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
type AssignedVar<'T> = AVar of 'T * Location

type VarStateGen<'T> = Scope<AssignedVar<'T> ,ASTFuncRef>
type VarState = VarStateGen<ASTVariable>
let vars x = x.variables
let funcs x = x.functions
type VarStateM<'t> = State<'t,VarState>

let getAssignedRegisters st = 
  List.choose 
    (function | (AVar (_, Reg x))  -> Some x | _ -> None) 
    st.variables

let addVariable v st : VarState = {st with variables = v :: st.variables}
let addVariables vs st : VarState = {st with variables = vs @ st.variables}

let assign loc var = AVar (var,loc) 
                  |> addVariable 
                  |> updateState 
                  
let callingConvention = seq {
  yield! List.map Reg [RDI;RSI;RDX;RCX;R8;R9]  
  yield! Seq.initInfinite (konst Stack)
}
  
//based on calling convention for C functions
let initScope a = a
               |> flip Seq.zip callingConvention
               |> Seq.map AVar
               |> Seq.toList
               |> addVariables
               |> updateState














