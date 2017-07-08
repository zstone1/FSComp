module ASTBuilder
 
open Parser
open FSharpx.State
open FSharpx

type Ty = IntTy
type Access = Public | Private

type ASTVariable = {
  ty : Ty
  originalName : string
  name : string
}

type ASTFuncRef = {
  ty : Ty
  name : string
  argTys : Ty list
}

type Scope = {
  uniqueNum : int
  variables : ASTVariable list
  functions : ASTFuncRef list
}

type private Scoped<'T> = State<'T,Scope>
let scope = state


type ASTExpression =
 | ASTLit of int
 | ASTVar of ASTVariable
 | ASTFunc of ASTFuncRef * (ASTExpression list)



type ASTStatement = 
  | ReturnStat of ASTExpression
  | IfStat of ASTExpression * ASTStatement list
  | Execution of ASTExpression 
  | Declaration of ASTVariable
  | Assignment of ASTVariable * ASTExpression
  | While of ASTExpression * ASTStatement list

type ASTSignature = {
  access : Access
  returnTy : Ty
  name : string
  args : ASTVariable list
}

type ASTFunction = {
  signature : ASTSignature
  body : ASTStatement list
}

let getType = function
  | ASTLit _ -> IntTy
  | ASTVar {ty = t} -> t
  | ASTFunc (t,_) -> t.ty

let findVarByOriginalName vorigninal scope = List.tryFind (fun {originalName = n} -> n = vorigninal) scope.variables

let findFunction name args scope = List.tryFind (fun ref -> args = ref.argTys && ref.name = name) scope.functions


let parseAccess = function
  | "public" -> Public
  | "private" -> Private
  | x -> failf "fail to parse access %s" x
  
let parseTy = function
  | "int" -> IntTy
  | x -> failf "fail to parse type %s" x
  
let uniqify originalName = scope {
  let! s = getState
  do! putState {s with uniqueNum = s.uniqueNum + 1}
  return sprintf "%s_%i" originalName s.uniqueNum
}

let convertSignature (s: FuncSignature) = scope {
  let argConverter (t, orig) = scope {
    let! newName = uniqify orig
    return { ty = parseTy t; originalName = orig; name = newName } }
  let! args = mapM argConverter s.args
  return { 
   access = s.access |> parseAccess
   returnTy = s.returnTy |> parseTy
   name = s.name
   args = args }
}

let hardCodedFunctions = [
  {ty = IntTy; name = PlusName; argTys = [IntTy; IntTy]};
  {ty = IntTy; name = MinusName; argTys = [IntTy; IntTy]};
  {ty = IntTy; name = MultName; argTys = [IntTy; IntTy]};
]

let rec convertExpr e scope = 
  match e with
  | IntLit x -> ASTLit x
  | Variable original as x -> 
    findVarByOriginalName original scope 
    |> function | Some v -> ASTVar v
                | None  -> failf "variable %s is not in scope" original
  | Func (v,args) as x -> 
    let convertedArgs = args |> List.map (fun e -> convertExpr e scope)
    match findFunction v (List.map getType convertedArgs) scope with 
    | Some t -> ASTFunc (t, convertedArgs)
    | None -> failf "function %A is not in scope" x


let introduceVariable ty name = scope {
   let! newName = uniqify name
   let decl = {ty = parseTy ty; originalName = name; name = newName}
   let updater st = {st with variables = decl :: st.variables}
   do! updateStateU updater
   return decl |> Declaration }

let convertControlStruct convertGuard guard convertBody body def = scope {
      let! guard = convertGuard guard <!> getState 
      let! before = getState 
      let! body =  mapM convertBody body |>> List.collect id
      let! after = getState
      do! putState {after with variables = before.variables}
      return def (guard, body) }

let convertDeclaration ty varName =  scope {
      let! s = getState
      return! match findVarByOriginalName ty s with
              | None -> introduceVariable ty varName |>> List.singleton
              | Some _ -> failf "variable %s is already in scope" varName }   

let convertAssignment varName expr = scope {
      let! s = getState
      let e' =  convertExpr expr s
      return match findVarByOriginalName varName s with
             | Some {ty = t1} when t1 <> (getType e') -> failf "variable %s has type %A, but expected %A" varName t1 e'
             | None -> failf "variable %s is not in scope" varName
             | (Some v) -> [(v,e') |> Assignment]}

let rec convertStatement (sgn: ASTSignature) = function
  | Parser.ReturnStat r -> 
        getState 
    |>> convertExpr r 
    |>> function | e when (e |> getType) = sgn.returnTy -> [e |> ReturnStat]
                 | _ -> failf "return %A didn't match expected %A" r sgn.returnTy
  
  | Parser.Declaration (t,o) -> convertDeclaration t o            
  | Parser.Execution e -> getState |>> (convertExpr e >> Execution >> List.singleton)
  
  | Parser.Assignment (o,e) -> convertAssignment o e
            
  //I need to ensure the scope is restored after popping out of the body
  | Parser.IfStat (e,stats) -> convertControlStruct convertExpr e (convertStatement sgn) stats IfStat
                           |>> List.singleton
  | Parser.While (e, stats) -> convertControlStruct convertExpr e (convertStatement sgn) stats While
                           |>> List.singleton
  |Parser.DeclAndAssign (ty, v, e) -> scope {
      let! decl = convertDeclaration ty v
      let! assign = convertAssignment v e 
      return decl @ assign } 
                                 

let convertFunction ({signature = sgn; body = body }:ParserFunction) = scope {
  let! sgn' = convertSignature sgn
  let! scopeInit = (fun s -> {s with variables = sgn'.args})  <!> getState
  do! putState scopeInit
  let! body' = mapM (convertStatement sgn') body |>> List.collect id
  return {signature = sgn'; body = body';}
}

let getFuncsInModule (fs :ParserFunction list) = 
  let getRef {ParserFunction.signature = s} = {
    ty = parseTy s.returnTy
    name = s.name
    argTys = List.map (parseTy << fst) s.args
  }
  List.map getRef fs

let public convertModule fs = 
  let signatures = getFuncsInModule fs
  let scopeInit = {
    uniqueNum = 0
    variables = []
    functions = hardCodedFunctions @ signatures
  }
  run (mapM convertFunction fs) scopeInit



 