module Flatten

open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type LabelMarker = LabelName of string

type Variable = VarName of string

type Atom = 
  | IntLitAtom of int
  | DataRefAtom of string
  | VarAtom of Variable

type Instruct = 
  | CmpI of Variable * Atom
  | AssignI of Variable * Atom
  | JnzI of LabelMarker
  | JmpI of LabelMarker
  | CallI of Variable option * LabelMarker * Atom list
  | ReturnI of Atom
  | LabelI of LabelMarker
  | AddI of Variable * Atom
  | SubI of Variable * Atom
  | IMulI of Variable * Atom

let mapInstruct f f' g h= function 
  | AddI (a,b) -> AddI (f a, g b)
  | CmpI (a,b) -> CmpI (f a, g b)
  | SubI (a,b) -> SubI (f a, g b)
  | IMulI (a,b) -> IMulI (f a, g b)
  | AssignI (a,b) -> AssignI (f a, g b)
  | JmpI (l) -> JmpI (h l)
  | JnzI (l) -> JnzI (h l)
  | CallI (v,l,args) -> CallI (f' v, h l, List.map g args)
  | LabelI (l) -> LabelI (h l)
  | ReturnI (v) -> ReturnI (g v)

type InterSt = {
  uniqueNum : int
  instructs : Instruct list
  stringLits : (string * string) list
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
  | ASTIntLit i -> i |> IntLitAtom |> returnM
  | ASTStringLit str -> state {
      let! name = makeNamePre "str"
      do! updateStateU (fun s -> {s with stringLits = (name, str) :: s.stringLits })
      return DataRefAtom name
    }
  | ASTVar v -> v.name |> VarName |> VarAtom |> returnM
  | ASTFunc ({name = PlusName; argTys = Some [IntTy;IntTy] }, [x;y]) ->
      handleArith AddI flattenExpression x y
  | ASTFunc ({name = MinusName; argTys = Some [IntTy;IntTy]}, [x;y]) -> 
      handleArith SubI flattenExpression x y
  | ASTFunc ({name = MultName; argTys = Some [IntTy; IntTy]}, [x;y]) ->
      handleArith IMulI flattenExpression x y
  | ASTFunc (s,args) -> state {
    let! args = mapM flattenExpression args
    let! rtnVar = makeVariable
    yield CallI (Some rtnVar, LabelName s.name, args)
    return rtnVar |> VarAtom }

let getGuardVar = 
  flattenExpression
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
    
let flattenFunc f = state {
  do! updateStateU (fun s -> {s with instructs = []})
  do! mapU flattenStatement f.body
  let! {instructs = x } = getState
  return (f.signature, x)
}

type FlattenedModule = {funcs : (ASTSignature * (Instruct list)) list; lits : (string * string) list}
let flattenModule fs (scope:Scope) = 
  let seed = { uniqueNum = scope.uniqueNum; instructs = []; stringLits = [] }
  fs 
  |> mapM flattenFunc
  |> fun s -> run s seed
  |> fun (funcs, state) -> {funcs = funcs; lits = state.stringLits}