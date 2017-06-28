module Assembly
open Parser
open VariableAssignment
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
type Instruction = 
  | Mov of Location * Location
  | Add of Location * Location
  | Sub of Location * Location
  | Ret 
  | Jnz of string 
  | Cmp of Location * Location
  | Label of string
  | Comment of string

let rec getExprLocation s = function 
  | IntLit i -> Imm i 
  | Variable v -> getVar v s |> getLocation
  | Func ("Add",[_;_]) | Func ("Sub",[_;_]) -> Reg RAX 

  | Func _ -> failf "Only add and sub are supported right now" 
  | StringLit _ -> failf "I'll deal with string constants later"

let getInstructionsExpr varState = function
  | IntLit i -> []
  | StringLit s -> []
  | Variable n ->  []
  | Func ("Add",[x;y]) -> [Add (getExprLocation varState x, getExprLocation varState y)]
  | Func ("Sub",[x;y]) -> [Sub (getExprLocation varState x, getExprLocation varState y)]
  | Func _ -> failf "Not supported function"


let exprToAssembly varState (_,e) = getInstructionsExpr varState e, getExprLocation varState e

let rec toInstructions (varState : VarState) e = 
  let instructs = exprToAssembly varState
  match e with
  | AReturnStat e-> let (is, l) = e |> instructs 
                    Ret :: Mov (Reg RAX, l) :: is
  | ADeclaration _ -> []
  | AAssignment (v, e) -> let x = Map.find v.name varState.variables
                          let (is, exprLoc) = e |> instructs 
                          Mov (getLocation x, exprLoc) :: is
  | AExecution e -> e |> instructs |> fst
  | AIfStat (n, g,b) ->
    let (is, guardLoc) = g |> instructs
    let bodyinstructs = b |> List.collect (toInstructions varState)
    Label n :: bodyinstructs @ Comment n :: (Jnz n) :: Cmp (Imm 0, guardLoc) :: is


//let funcToInstructions f varState = 