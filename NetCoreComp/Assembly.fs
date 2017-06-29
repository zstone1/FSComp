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
  | Syscall
  | Comment of string 

let serializeLocation rspOffset = function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | OnStack i -> rspOffset - i |> sprintf "qword [rsp + %i]" //stackgrowsdown.com

let serializeInstruction rspOffset = 
  let serialize = serializeLocation rspOffset 
  function 
  | Mov (l1,l2) -> sprintf "        mov    %s, %s" (serialize l1) (serialize l2)
  | Add (l1,l2) -> sprintf "        add    %s, %s" (serialize l1) (serialize l2)
  | Sub (l1,l2) -> sprintf "        sub    %s, %s" (serialize l1) (serialize l2)
  | Cmp (l1,l2) -> sprintf "        cmp    %s, %s" (serialize l1) (serialize l2)
  | Ret -> sprintf "        ret"
  | Jnz l -> sprintf "        jnz    %s" l
  | Label l -> sprintf "%s:" l
  | Comment c -> sprintf "        ; %s" c
  | Syscall -> "        syscall"

let rec getExprLocation s = function 
  | IntLit i -> Imm i 
  | Variable v -> getVar v s |> getLocation
  | Func ("Add",[_;_]) | Func ("Sub",[_;_]) -> Reg RAX 

  | Func (a,b) -> failf "Only add and sub are supported right now. You had %A" (a,b)
  | StringLit _ -> failf "I'll deal with string constants later"

let handleArithmetic varState instr o1 o2 = [Mov (Reg RAX, o1); instr (Reg RAX, o2)], Reg RAX
  

let getInstructionsExpr varState = function
  | IntLit i -> ([], Imm i)
  | StringLit s -> failf "No strings yet"
  | Variable v -> ([], getVar v varState |> getLocation)
  | Func ("Add",[x;y]) -> handleArithmetic varState Add  (getExprLocation varState x) (getExprLocation varState y)
  | Func ("Sub",[x;y]) -> handleArithmetic varState Sub (getExprLocation varState x) (getExprLocation varState y)
  | Func _ -> failf "Not supported function"


let exprToAssembly varState (_,e) = getInstructionsExpr varState e, getExprLocation varState e

let rec toInstructions rspOffset (varState : VarState) e = 
  let instructs (_,e) = getInstructionsExpr varState e
  match e with
  | AReturnStat e-> let (is, l) = e |> instructs 
                    is @ [Mov (Reg RDI, l); Mov (Reg RAX, Imm 60); Syscall]
  | ADeclaration _ -> []
  | AAssignment (v, e) -> let x = Map.find v.name varState.variables
                          let (is, exprLoc) = e |> instructs 
                          is @ [Mov (getLocation x, exprLoc)]
  | AExecution e -> e |> instructs |> fst
  | AIfStat (n, g,b) ->
    let (is, guardLoc) = g |> instructs
    let bodyinstructs = b |> List.collect (toInstructions rspOffset varState)
    is @ [Cmp (guardLoc, Imm 0); (Jnz n)] @ bodyinstructs @ [Label n]

let getRspOffset ({variables = v}:VarState) = 
  v 
  |> Seq.map (fun i -> match i.Value with | AVar (_, OnStack i) -> i | _ -> 0)
  |> Seq.max

let funcToInstructions stats (varState : VarState) = 
  let rspOffset = getRspOffset varState + 8
  let init = [Sub (Reg RSP, Imm rspOffset)]
  let final = [Add (Reg RSP, Imm rspOffset)]
  let body = stats |> List.collect (toInstructions rspOffset varState)
  init @ body 
  |> List.map (serializeInstruction rspOffset)
  |> String.concat "\n"

let serializeModule fs = 
  match fs with
  | [] -> failf "no functions?"
  | x::y::xs -> failf "too many functions"
  | [x] -> let body = x ||> funcToInstructions 
           sprintf "        global _start\n \n        section .text \n_start:\n%s" body
