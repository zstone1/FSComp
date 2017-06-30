module VariableAssignment

open Parser
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type Label = Label of string

type Atom = 
  | IntLitAtom of int
  | VarName of string

type InterValue = 
  | AtomV of Atom
  | FuncV of string * (Atom list)

type Instructs = 
  | CmpI of Atom * Atom
  | AssignI of string * InterValue
  | JNZI of Label
  | ReturnI of Atom

type Context = {
  uniqueNum : int
  instructs : Instructs list
}

let addInstruct i = updateState' (fun s -> {s with instructs = i :: s.instructs})
let Cmp a1 a2 = CmpI (a1,a2) |> addInstruct
let Assign a1 a2 = AssignI (a1, a2) |> addInstruct
let Jnz l = JNZI l |> addInstruct
let Return a = ReturnI a |> addInstruct


let rec flattenExpression = function
  | ASTLit i -> i |> IntLitAtom |> AtomV |> returnM
  | ASTVar v -> v.name |> VarName |> AtomV |> returnM
  | ASTFunc (ref,v) -> state {
    let! args = mapM flattenArg v
    return (ref.name,args) |> FuncV
  }
and flattenArg = function
    | ASTFunc _ as f' -> state {
      let! s = getState 
      do! putState {s with uniqueNum = s.uniqueNum + 1}
      let tempName = sprintf "_temp_%i" s.uniqueNum
      let! flattened = flattenExpression f'
      do! Assign tempName flattened
      return VarName tempName}
    | ASTLit i -> i |> IntLitAtom |> returnM
    | ASTVar v -> v.name |> VarName |> returnM
 