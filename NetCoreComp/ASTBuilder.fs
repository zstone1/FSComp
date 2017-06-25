module ASTBuilder
 
open Parser
open FSharpx.State
open FSharpx

type Ty = IntTy | StringTy
type Access = Public | Private

type ASTVariable = Var of Ty * string
type ASTFuncRef = FuncRef of Ty * string * Ty list

type Scope<'V,'F> = {
  variables : 'V list
  functions : 'F list
}

type private Scoped<'T> = State<'T,Scope<ASTVariable, ASTFuncRef>>
let scope = state

let findVariable' v vars = List.tryFind (fun (Var (_,n)) -> n = v) vars
let findVariable v scope = findVariable' v scope.variables
let findFunction' name args funcs = List.tryFind (fun (FuncRef(_,n,l)) -> n = name && args = l) funcs
let findFunction name args scope = findFunction' name args scope.functions

type ASTExpression = Ty * Expression

let name (Var (_,n)) = n
 
type ASTStatement = 
  | ReturnStat of ASTExpression
  | IfStat of ASTExpression * ASTStatement list
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
}

let parseAccess = function
  | "public" -> Public
  | "private" -> Private
  | x -> failf "fail to parse access %s" x
  
let parseTy = function
  | "int" -> IntTy
  | "string" -> StringTy
  | x -> failf "fail to parse type %s" x
  

let convertSignature (s: FuncSignature) = { 
    access = parseAccess s.access 
    isStatic = s.isStatic
    returnTy = parseTy s.returnTy
    name = s.name
    args = List.map (fun (a,b) -> Var (parseTy a,b)) s.args }

let hardCodedFunctions = [
  FuncRef(IntTy, "Add", [IntTy; IntTy]); 
  FuncRef(IntTy, "Sub", [IntTy; IntTy])]

let introduceVariable ty name = scope {
    let decl =  (parseTy ty, name) |> Var
    let updater st = {st with variables = decl :: st.variables}
    do! updateState updater |>> ignore
    return decl |> Declaration }
 
let rec convertExpr scope = function
  | IntLit _ as x -> (IntTy, x)
  | StringLit _ as x -> (StringTy, x)
  | Variable v as x -> 
       v
    |> flip findVariable scope 
    |> function | Some (Var (t,_)) -> (t,x)
                | None  -> failf "variable %s is not in scope" v
  | Func (v,args) as x -> 
       args 
    |> List.map (convertExpr scope >> fst)
    |> flip3' findFunction v scope 
    |> function | Some (FuncRef (ty,_,_)) -> (ty, x)
                | None -> failf "function %A is not in scope" x

let rec convertExpr' = flip convertExpr
   
let rec convertStatement (sgn: ASTSignature) = function
  | Parser.ReturnStat r -> 
        getState 
    |>> convertExpr' r 
    |>> function | (ty,x) when ty = sgn.returnTy -> (ty,x) |> ReturnStat
                 | _ -> failf "return %A didn't match expected %A" r sgn.returnTy
  
  | Parser.Declaration (t,s) -> 
        getState 
    |>> findVariable s 
    >>= function | None -> introduceVariable t s
                 | Some _ -> failf "variable %s is already in scope" s
              
  | Parser.Execution e -> getState |>> (convertExpr' e >> Execution)
  
  | Parser.Assignment (s,e) ->  scope {
      let! e' =  convertExpr' e <!> getState 
      let! found = findVariable s <!> getState 
      return match (found, e') with
             | Some ((Var (t1, _)) as v), (t2,_) when t1 = t2 ->  (v,e') |> Assignment
             | Some (Var (t1,_)), (t2,_) -> failf "variable %s has type %A, but expected %A" s t1 t2
             | None, _ -> failf "variable %s is not in scope" s }
            
  //I need to ensure the scope is restored after popping out of the body
  | Parser.IfStat (e,stats) -> scope {
      let! guard = convertExpr' e <!> getState 
      let! before = getState 
      let! body =  mapM (convertStatement sgn) stats
      do! putState before
      return IfStat (guard, body) }

let convertFunction ({signature = sgn; body = body }:ParserFunction) : ASTFunction =
  let sgn' = convertSignature sgn
//  let thisFunc = FuncRef (sgn'.returnTy, sgn'.name, List.map fst sgn'.args) //For recursion
  let scopeInit = {
    variables = sgn'.args 
    functions = hardCodedFunctions//thisFunc :: hardCodedFunctions
  }
  let body' = mapM (convertStatement sgn') body |> flip eval scopeInit 
  {signature = sgn'; body =body'}

let public convertModule = List.map convertFunction


 