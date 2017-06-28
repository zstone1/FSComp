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
  | Jnz of Location 
  | Cmp of Location * Location

let getInstructionsExpr : ASTExpression -> Instruction list * Location = failf "foo"
let rec toInstructions (varState : VarState) = function
  | AReturnStat e-> let (is, l) = getInstructionsExpr e 
                    Ret :: Mov (Reg RAX, l) :: is
  | ADeclaration _ -> []
  | AAssignment (v, e) -> let x = Map.find v.name varState.variables
                          let (is, exprLoc) = getInstructionsExpr e
                          Mov (getLocation x, exprLoc) :: is
  | AExecution e -> e |> getInstructionsExpr |> fst
  | AIfStat (g,b) -> 
    let (is, guardLoc) = g |> getInstructionsExpr
    let bodyinstructs = b |> List.collect (toInstructions varState)
    []


