module Flatten

open Parser
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type LabelMarker = LabelName of string

type Atom = 
  | IntLitAtom of int
  | VarName of string

type Instruct = 
  | CmpI of Atom * Atom
  | AssignI of string * Atom
  | JNZI of LabelMarker
//  | CallI of Atom * string * Atom list
  | ReturnI of Atom
  | LabelI of LabelMarker
  | AddI of Atom * Atom
  | SubI of Atom * Atom

type InterSt = {
  uniqueNum : int
  instructs : Instruct list
}

let addInstruct i = updateState' (fun s -> {s with instructs = s.instructs @ [i]})
let Cmp a1 a2 = CmpI (a1,a2) |> addInstruct
let Assign a1 a2 = AssignI (a1, a2) |> addInstruct
let Jnz l = JNZI l |> addInstruct
let Return a = ReturnI a |> addInstruct
//let Call a b c = CallI (a,b,c) |> addInstruct
let Label l = LabelI l |> addInstruct
let Add a1 a2 = AddI (a1,a2) |> addInstruct

let Sub a1 a2 = SubI (a1,a2) |> addInstruct


let makeNamePre prefix : State<string,InterSt> = state {
    let! s = getState 
    do! putState {s with uniqueNum = s.uniqueNum + 1}
    return sprintf "_%s_%i" prefix s.uniqueNum}

let makeName = makeNamePre "temp" 

let handleArith arith flatten a b = state {
    let! newVar = makeName
    let! aloc = flatten a
    do! Assign newVar aloc
    let! bloc = flatten b
    do! arith (VarName newVar) bloc
    return  newVar |> VarName }

let rec flattenExpression = function
  | ASTLit i -> i |> IntLitAtom |> returnM
  | ASTVar v -> v.name |> VarName |> returnM
  | ASTFunc ({name = PlusName; argTys = [IntTy;IntTy]}, [x;y]) ->
      handleArith Add flattenExpression x y
  | ASTFunc ({name = MinusName; argTys = [IntTy;IntTy]}, [x;y]) -> 
      handleArith Sub flattenExpression x y
  | ASTFunc (_,_) -> failf "only add and sub are supported"
//  | ASTFunc ({name = n},v) -> state {
//    let! args = mapM flattenExpression v
//    let! rtnName = makeName |>> VarName
//    do! Call rtnName n args
//    return rtnName }

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
    
let flattenFunc fs n = 
  let init = {
    uniqueNum = n
    instructs = []
  }
  let work = Label (LabelName fs.signature.name)
          *> mapMUnit flattenStatement fs.body
  exec work init

let flattenModule fs (scope:Scope) = 
  let seed = scope.uniqueNum
  let accum (acc,i) f =
     let {instructs = l; uniqueNum = i'} = flattenFunc f i
     (l :: acc, i')
  List.fold accum ([],scope.uniqueNum) fs