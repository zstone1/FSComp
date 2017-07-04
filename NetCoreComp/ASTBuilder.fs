module ASTBuilder
 
open Parser
open FSharpx.State
open FSharpx

type Ty = IntTy | StringTy
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

let findVariableByOriginal vorigninal scope = List.tryFind (fun {originalName = n} -> n = vorigninal) scope.variables

let findFunction' name args funcs = List.tryFind (fun (ref) -> ref.name = name && args = ref.argTys) funcs
let findFunction name args scope = findFunction' name args scope.functions

type ASTExpression =
 | ASTLit of int
 | ASTVar of ASTVariable
 | ASTFunc of ASTFuncRef * (ASTExpression list)

let getType = function
  | ASTLit _ -> IntTy
  | ASTVar {ty = t} -> t
  | ASTFunc (t,_) -> t.ty

let name ({name = n}:ASTVariable) = n
 
type ASTStatement = 
  | ReturnStat of ASTExpression
  | IfStat of string * ASTExpression * ASTStatement list
  | Execution of ASTExpression 
  | Declaration of ASTVariable
  | Assignment of ASTVariable * ASTExpression
  | While of string * ASTExpression * ASTStatement list
 
type ASTSignature = {
  access : Access
  isStatic : bool
  returnTy : Ty
  name : string
  args : ASTVariable list
}

type ASTFunction = {
  signature : ASTSignature
  body : ASTStatement list
}

let parseAccess = function
  | "public" -> Public
  | "private" -> Private
  | x -> failf "fail to parse access %s" x
  
let parseTy = function
  | "int" -> IntTy
  | "string" -> StringTy
  | x -> failf "fail to parse type %s" x
  
let uniqify originalName = scope {
  let! s = getState
  do! putState {s with uniqueNum = s.uniqueNum + 1}
  return sprintf "%s_%i" originalName s.uniqueNum
}

let argConverter orig t = scope {
  let! newName = uniqify orig
  return { ty = parseTy t; originalName = orig; name = newName }
}

let convertSignature (s: FuncSignature) = scope {
  let! args = mapM (uncurry argConverter) s.args
  return { 
   access = parseAccess s.access 
   isStatic = s.isStatic
   returnTy = parseTy s.returnTy
   name = s.name
   args = args }
}

let hardCodedFunctions = [
  {ty = IntTy; name = PlusName; argTys = [IntTy; IntTy]};
  {ty = IntTy; name = MinusName; argTys = [IntTy; IntTy]};
]

let rec convertExpr scope = function
  | IntLit x -> ASTLit x
  | StringLit _ as x -> failf "strings not supported yet"
  | Variable original as x -> 
    findVariableByOriginal original scope 
    |> function | Some v -> ASTVar v
                | None  -> failf "variable %s is not in scope" original
  | Func (v,args) as x -> 
    let newArgs = args |> List.map (convertExpr scope)
    match findFunction v (List.map getType newArgs) scope with 
    | Some t -> ASTFunc (t, newArgs)
    | None -> failf "function %A is not in scope" x

let rec convertExpr' = flip convertExpr

let introduceVariable ty name = scope {
   let! newName = uniqify name
   let decl = {ty = parseTy ty; originalName = name; name = newName}
   let updater st = {st with variables = decl :: st.variables}
   do! updateState updater |>> ignore
   return decl |> Declaration }

let convertControlStruct convertBody convertGuard def guard body = scope {
      let! guard = convertGuard guard <!> getState 
      let! before = getState 
      let! body =  mapM convertBody body |>> List.collect id
      let! after = getState
      do! putState {after with variables = before.variables}
      let name = sprintf "label_%i" after.uniqueNum
      return def (name, guard, body) }

let convertDeclaration ty varName =  scope {
      let! s = getState
      let var = findVariableByOriginal ty s
      match var with 
      | None -> return! introduceVariable ty varName |>> List.singleton
      | Some _ -> return failf "variable %s is already in scope" varName }   

let convertAssignment varName expr = scope {
      let! e' =  convertExpr' expr <!> getState 
      let! found = findVariableByOriginal varName <!> getState 
      return match found with
             | Some {ty = t1} when t1 <> (getType e') -> failf "variable %s has type %A, but expected %A" varName t1 e'
             | None -> failf "variable %s is not in scope" varName
             | (Some v) -> [(v,e') |> Assignment]}

let rec convertStatement (sgn: ASTSignature) = function
  | Parser.ReturnStat r -> 
        getState 
    |>> convertExpr' r 
    |>> function | e when (e |> getType) = sgn.returnTy -> [e |> ReturnStat]
                 | _ -> failf "return %A didn't match expected %A" r sgn.returnTy
  
  | Parser.Declaration (t,o) -> convertDeclaration t o            
  | Parser.Execution e -> getState |>> (convertExpr' e >> Execution) |>> List.singleton
  
  | Parser.Assignment (o,e) -> convertAssignment o e
            
  //I need to ensure the scope is restored after popping out of the body
  | Parser.IfStat (e,stats) -> convertControlStruct 
                                 (convertStatement sgn) 
                                 convertExpr'
                                 (IfStat >> List.singleton)
                                 e 
                                 stats
  | Parser.While (e, stats) -> convertControlStruct
                                (convertStatement sgn)
                                convertExpr'
                                (While >> List.singleton)
                                e
                                stats
  |Parser.DeclAndAssign (ty, v, e) -> scope {
      let! decl = convertDeclaration ty v
      let! assign = convertAssignment v e 
      return decl @ assign } 
                                 

let convertFunction ({signature = sgn; body = body }:ParserFunction) = scope {
  let! sgn' = convertSignature sgn
  let! uniqueNum = (fun i -> i.uniqueNum) <!> getState
  let! funcs = (fun i -> i.functions) <!> getState
  let scopeInit = {
    uniqueNum = uniqueNum
    variables = sgn'.args 
    functions = funcs
  }
  do! putState scopeInit
  let! body' = mapM (convertStatement sgn') body |>> List.collect id
  let! s = getState
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



 