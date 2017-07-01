module VariableAssignment

open Parser
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type LabelMarker = LabelName of string

type Atom = 
  | IntLitAtom of int
  | VarName of string

type Instructs = 
  | CmpI of Atom * Atom
  | AssignI of string * Atom
  | JNZI of LabelMarker
  | CallI of string * Atom list
  | ReturnI of Atom
  | LabelI of LabelMarker

type InterSt = {
  uniqueNum : int
  instructs : Instructs list
}

let addInstruct i = updateState' (fun s -> {s with instructs = i :: s.instructs})
let Cmp a1 a2 = CmpI (a1,a2) |> addInstruct
let Assign a1 a2 = AssignI (a1, a2) |> addInstruct
let Jnz l = JNZI l |> addInstruct
let Return a = ReturnI a |> addInstruct
let Call exs = CallI exs |> addInstruct
let Label l = LabelI l |> addInstruct

let makeNamePre prefix : State<string,InterSt> = state {
    let! s = getState 
    do! putState {s with uniqueNum = s.uniqueNum + 1}
    return sprintf "_%s_%i" prefix s.uniqueNum}

let makeName = makeNamePre "temp" 

let rec flattenExpression = function
  | ASTLit i -> i |> IntLitAtom |> returnM
  | ASTVar v -> v.name |> VarName |> returnM
  | ASTFunc (ref,v) -> state {
    let! args = mapM flattenExpression v
    let! rtnName = makeName
    do! Call (rtnName, args)
    return rtnName |> VarName }

let rec flattenStatement = function
  | ReturnStat e -> Return =<< flattenExpression e 
  | Declaration _ -> empty
  | Execution e -> flattenExpression e |>> ignore
  | Assignment ({name = n},e) -> Assign n =<< flattenExpression e
  | IfStat (label, guard, body) -> state {
      let! guardVar = flattenExpression guard
      let! label = makeNamePre "lab" |>> LabelName
      do! Cmp guardVar (IntLitAtom 0)
      do! Jnz label
      do! mapMUnit flattenStatement body 
      do! Label label
      return () }
    