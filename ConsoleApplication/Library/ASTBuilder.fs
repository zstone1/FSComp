module ASTBuilder
open Parser

type Ty = IntTy | StringTy
  
type Access = Public | Private

type Scope = {
  variables : (Ty * string) list
  functions : (Ty * string * Ty list) list
}

let findVariable scope v = List.tryFind (fun (_,n) -> n = v) scope.variables 

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

let rec convertExpr (scope: Scope) = function
  | IntLit _ as x -> (IntTy, x)
  | StringLit _ as x -> (StringTy, x)
  | Variable v as x -> match findVariable scope v with
                       | Some (t,_) -> (t,x)
                       | None  -> raise (CompilerError (sprintf "variable %s is not in scope" v))
  | Func (v,args) as x -> let args' = List.map (convertExpr scope) args
                          let argMatch (_, name, candArgs) = name = v 
                                                          && List.length candArgs = List.length args'
                          match List.tryFind argMatch scope.functions with
                          | Some (ty,_,_) -> (ty, x)
                          | None -> raise (CompilerError (sprintf "function %A is not in scope" x))

//TODO: make this tail recurisve with a state monad.
let rec convertBody (scope: Scope) = function
  | Parser.ReturnStat r::[] -> [convertExpr scope r |> ReturnStat]
  | Parser.Declaration (t,s) :: xs ->
    match findVariable scope s with
    | Some _ -> failComp <| sprintf "variable %s is already in scope" s
    | None -> Declaration (parseTy t, s) :: convertBody {scope with variables = (parseTy t,s) :: scope.variables} xs
  | Parser.Execution e :: xs -> (convertExpr scope e |> Execution) :: convertBody scope xs
  | Parser.Assignment (s,e) :: xs -> 
    match findVariable scope s, convertExpr scope e with
    | Some (varTy,_), ((exprTy,_) as e') when varTy = exprTy ->  Assignment (s,e') :: convertBody scope xs
    | Some (varTy,_), (exprTy,_) -> failComp <| sprintf "variable %s has type %A, but expected %A" s varTy exprTy
    | None, _ -> failComp <| sprintf "variable %s is not in scope" s
  | Parser.IfStat (e,stats) :: xs -> IfStat(convertExpr scope e, convertBody scope stats) :: convertBody scope xs
                                        
  | [] -> raise (CompilerError (sprintf "no return statement at end of this function"))
  | Parser.ReturnStat r::(x::xs) -> raise (CompilerError (sprintf "return statement not and end of clause"))

let convertFunction ({signature = sgn; body = body }:ParserFunction) : ASTFunction =
  let sgn' = convertSignature sgn
  let scope = {variables = sgn'.args; functions = [(sgn'.returnTy, sgn'.name, List.map fst sgn'.args)]}
  let body' = convertBody scope body
  {signature = sgn'; body =body'}

let public convertModule = List.map convertFunction


 