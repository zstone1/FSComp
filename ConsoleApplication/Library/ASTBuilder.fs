module ASTBuilder
open Parser
open FSharpx.State
open FSharpx.Functional

type Ty = IntTy | StringTy
  
type Access = Public | Private

type Scope = {
  variables : (Ty * string) list
  functions : (Ty * string * Ty list) list
}

type Scoped<'T> = State<'T,Scope>
let scope = state

let findVariable v scope = List.tryFind (fun (_,n) -> n = v) scope.variables
let findFunction name args scope = List.tryFind (fun (_,n,l) -> n = name && args = l) scope.functions

type ASTExpression = Ty * Expression
 
type ASTStatement = 
 | ReturnStat of ASTExpression
 | IfStat of ASTExpression * ASTStatement list
 | Execution of ASTExpression 
 | Declaration of Ty * string
 | Assignment of string * ASTExpression
 
type ASTSignature = {
  access : Access
  isStatic : bool
  returnTy : Ty
  name : string
  args : (Ty * string) list
}

type ASTFunction = {
  signature : ASTSignature
  body : ASTStatement list
}

let parseAccess = function
  | "public" -> Public
  | "private" -> Private
  | x -> raise (CompilerError (sprintf "fail to parse access %s" x))
  
let parseTy = function
  | "int" -> IntTy
  | "string" -> StringTy
  | x -> raise (CompilerError (sprintf "fail to parse type %s" x))
  

let convertSignature (s: FuncSignature) = { 
    access = parseAccess s.access 
    isStatic = s.isStatic
    returnTy = parseTy s.returnTy
    name = s.name
    args = List.map (fun (a,b) -> (parseTy a,b)) s.args }

let hardCodedFunctions = [(IntTy, "Add", [IntTy; IntTy]); (IntTy, "Sub", [IntTy; IntTy])]

let rec convertExpr e scope = 
  match e with
  | IntLit _ as x -> (IntTy, x)
  | StringLit _ as x -> (StringTy, x)
  | Variable v as x ->
     match findVariable v scope with
     | Some (t,_) -> (t,x)
     | None  -> failf "variable %s is not in scope" v
  | Func (v,args) as x -> 
    let args' = List.map (flip convertExpr scope >> fst) args
    match findFunction v args' scope with
    | Some (ty,_,_) -> (ty, x)
    | None -> failf "function %A is not in scope" x

let rec convertStatement (sgn: ASTSignature) = function
  | Parser.ReturnStat r -> scope {
      let! (ty, r') as e' = convertExpr r <!> getState 
      return match ty = sgn.returnTy with
             | true -> [ReturnStat e'] 
             | false -> failf "return %A didn't match expected %A" r sgn.returnTy }
  
  | Parser.Declaration (t,s) -> scope {
      let! x = findVariable s <!> getState
      match x with
      | None -> let decl = parseTy t, s
                do! updateState (fun st -> {st with variables = decl :: st.variables}) |> ignoreM
                return [Declaration decl]
      | Some _ -> return failf "variable %s is already in scope" s }
      
              
  | Parser.Execution e -> convertExpr e >> Execution >> List.singleton <!> getState 
  
  | Parser.Assignment (s,e) ->  scope {
      let! e' = convertExpr e <!> getState
      let! found = findVariable s <!> getState
      return match (found, e') with
             | Some (t1,_), (t2,_) when t1 = t2 ->  [Assignment (s,e')]
             | Some (t1,_), (t2,_) -> failf "variable %s has type %A, but expected %A" s t1 t2
             | None, _ -> failf "variable %s is not in scope" s }
            
  | Parser.IfStat (e,stats) -> scope {
      let! guard = convertExpr e <!> getState 
      let! before = getState //I need to ensure the scope is restored after popping out of the boyd
      let! body = List.concat <!> mapM (convertStatement sgn) stats
      do! putState before
      return [IfStat (guard, body)] }

let convertFunction ({signature = sgn; body = body }:ParserFunction) : ASTFunction =
  let sgn' = convertSignature sgn
  let thisFunc = (sgn'.returnTy, sgn'.name, List.map fst sgn'.args) //For recursion
  let scopeInit = {variables = sgn'.args; functions = thisFunc :: hardCodedFunctions}
  let body' = mapM (convertStatement sgn') body |> flip eval scopeInit |> List.concat
  {signature = sgn'; body =body'}

let public convertModule = List.map convertFunction


 