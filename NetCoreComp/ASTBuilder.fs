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

type ASTFuncRef = FuncRef of Ty * string * Ty list

type Scope = {
  uniqueNum : int
  variableCount : int
  variables : ASTVariable list
  functions : ASTFuncRef list
}

type private Scoped<'T> = State<'T,Scope>
let scope = state

let findVariableByOriginal vorigninal scope = List.tryFind (fun {originalName = n} -> n = vorigninal) scope.variables

let findFunction' name args funcs = List.tryFind (fun (FuncRef(_,n,l)) -> n = name && args = l) funcs
let findFunction name args scope = findFunction' name args scope.functions

type ASTExpression = Ty * Expression

let name ({name = n}:ASTVariable) = n
 
type ASTStatement = 
  | ReturnStat of ASTExpression
  | IfStat of string * ASTExpression * ASTStatement list
  | Execution of ASTExpression 
  | Declaration of ASTVariable
  | Assignment of ASTVariable * ASTExpression
 
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
  locals : int
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
  do! putState {s with uniqueNum = s.uniqueNum + 1; variableCount = s.variableCount + 1}
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
  FuncRef(IntTy, "Add", [IntTy; IntTy]); 
  FuncRef(IntTy, "Sub", [IntTy; IntTy])]


let rec convertExpr scope = function
  | IntLit _ as x -> (IntTy, x)
  | StringLit _ as x -> (StringTy, x)
  | Variable original as x -> 
    findVariableByOriginal original scope 
    |> function | Some {name = n; ty = t} -> (t, Variable n)
                | None  -> failf "variable %s is not in scope" original
  | Func (v,args) as x -> 
       args 
    |> List.map (convertExpr scope >> fst)
    |> flip3' findFunction v scope 
    |> function | Some (FuncRef (ty,_,_)) -> (ty, x)
                | None -> failf "function %A is not in scope" x

let rec convertExpr' = flip convertExpr

let introduceVariable ty name = scope {
   let! newName = uniqify name
   let decl = {ty = parseTy ty; originalName = name; name = newName}
   let updater st = {st with variables = decl :: st.variables}
   do! updateState updater |>> ignore
   return decl |> Declaration }
   
let rec convertStatement (sgn: ASTSignature) = function
  | Parser.ReturnStat r -> 
        getState 
    |>> convertExpr' r 
    |>> function | (ty,x) when ty = sgn.returnTy -> (ty,x) |> ReturnStat
                 | _ -> failf "return %A didn't match expected %A" r sgn.returnTy
  
  | Parser.Declaration (t,o) -> scope {
      let! s = getState
      let var = findVariableByOriginal o s
      match var with 
      | None -> return! introduceVariable t o
      | Some _ -> return failf "variable %s is already in scope" o }
              
  | Parser.Execution e -> getState |>> (convertExpr' e >> Execution)
  
  | Parser.Assignment (o,e) ->  scope {
      let! e' =  convertExpr' e <!> getState 
      let! found = findVariableByOriginal o <!> getState 
      return match (found, e') with
             | Some {ty = t1}, (t2,_) when t1 <> t2 -> failf "variable %s has type %A, but expected %A" o t1 t2 
             | None, _ -> failf "variable %s is not in scope" o 
             | (Some v), x -> (v,x) |> Assignment}
            
  //I need to ensure the scope is restored after popping out of the body
  | Parser.IfStat (e,stats) -> scope {
      let! guard = convertExpr' e <!> getState 
      let! before = getState 
      let! body =  mapM (convertStatement sgn) stats
      let! after = getState
      do! putState {after with variables = before.variables}
      let name = sprintf "label_%i" after.uniqueNum
      return IfStat (name, guard, body) }

let convertFunction ({signature = sgn; body = body }:ParserFunction) = scope {
  let! sgn' = convertSignature sgn
  let! uniqueNum = (fun i -> i.uniqueNum) <!> getState
  let scopeInit = {
    uniqueNum = uniqueNum
    variableCount = 0
    variables = sgn'.args 
    functions = hardCodedFunctions
  }
  do! putState scopeInit
  let! body' = mapM (convertStatement sgn') body
  let! s = getState
  return {signature = sgn'; body = body'; locals = s.variableCount}
}

let public convertModule fs= 
  let scopeInit = {
    uniqueNum = 0
    variableCount = 0
    variables = []
    functions = hardCodedFunctions
  }
  run (mapM convertFunction fs) scopeInit



 