module Flatten

open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type LabelMarker = LabelName of string

type Variable = VarName of string

type Atom = 
  | IntLitAtom of int
  | VarAtom of Variable

type Instruct = 
  | CmpI of Variable * Atom
  | AssignI of Variable * Atom
  | JnzI of LabelMarker
  | JmpI of LabelMarker
  | CallI of Variable * LabelMarker * Atom list
  | ReturnI of Atom
  | LabelI of LabelMarker
  | AddI of Variable * Atom
  | SubI of Variable * Atom

type InterSt = {
  uniqueNum : int
  instructs : Instruct list
}

type StateBuilder with 
  member x.Yield(i) = updateStateU (fun s -> {s with InterSt.instructs = s.instructs @ [i]})

let makeNamePre prefix : State<string,InterSt> = state {
    let! s = getState 
    do! putState {s with uniqueNum = s.uniqueNum + 1}
    return sprintf "_%s_%i" prefix s.uniqueNum}

let makeVariable = makeNamePre "temp" |>> VarName

let handleArith arith flatten x y = state {
    let! xVar = makeVariable
    let! xloc = flatten x
    yield AssignI (xVar, xloc)
    let! yloc = flatten y
    yield arith (xVar, yloc)
    return xVar |> VarAtom}

let rec flattenExpression = function
  | ASTLit i -> i |> IntLitAtom |> returnM
  | ASTVar v -> v.name |> VarName |> VarAtom |> returnM
  | ASTFunc ({name = PlusName; argTys = [IntTy;IntTy]}, [x;y]) ->
      handleArith AddI flattenExpression x y
  | ASTFunc ({name = MinusName; argTys = [IntTy;IntTy]}, [x;y]) -> 
      handleArith SubI flattenExpression x y
  | ASTFunc (s,args) -> state {
    let! args = mapM flattenExpression args
    let! rtnVar = makeVariable
    yield CallI (rtnVar, LabelName s.name, args)
    return rtnVar |> VarAtom }

let getGuardVar = flattenExpression
              >=> function 
                  | VarAtom v -> v |> returnM
                  | x -> state {
                       let! tempVar = makeVariable
                       yield AssignI (tempVar, x)
                       return tempVar }

let rec flattenStatement = function
  | ReturnStat e -> state { let! flatE = flattenExpression e
                            yield ReturnI flatE}
  | Declaration _ -> empty
  | Execution e -> flattenExpression e |>> ignore
  | Assignment ({name = n},e) -> state {let! flatE = flattenExpression e
                                        yield AssignI (VarName n, flatE) }
  | IfStat (guard, body) -> state {
      let! guardVar = getGuardVar guard
      let! skipIf = makeNamePre "if_lab" |>> LabelName
      yield CmpI (guardVar, IntLitAtom 0)
      yield JnzI skipIf
      do! mapU flattenStatement body 
      yield LabelI skipIf
      return () }
  | While (guard, body) -> state {
      let! guardVar = getGuardVar guard
      let! startLab = makeNamePre "while_start_lab" |>> LabelName
      let! endLab = makeNamePre "while_end_lab" |>> LabelName
      yield LabelI startLab
      yield CmpI (guardVar, IntLitAtom 0)
      yield JnzI endLab
      do! mapU flattenStatement body
      yield JmpI startLab
      yield LabelI endLab }
    
let flattenFunc fs n = 
  let init = {
    uniqueNum = n
    instructs = []
  }
  let work = mapU flattenStatement fs.body
  exec work init

let flattenModule fs (scope:Scope) = 
  let seed = scope.uniqueNum
  let accum (acc,i) f =
     let {instructs = l; uniqueNum = i'} = flattenFunc f i
     ((f.signature,l) :: acc, i')
  List.fold accum ([],scope.uniqueNum) fs