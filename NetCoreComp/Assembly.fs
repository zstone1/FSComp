module Assembly
open Parser
open VariableAssignment
open ASTBuilder
open FSharp.Collections;
open FSharpx
open FSharpx.State
 
 (*
type Instruction = 
  | Mov of Location * Location
  | Add of Location * Location
  | Sub of Location * Location
  | Ret 
  | Jnz of string 
  | Cmp of Location * Location
  | Syscall
  | Push of Location
  | Pop of Location

type AssemblyState = {
  variableLocations : Map<string,Location>
  instructions : string
  rspOffset : int
}

let assembly = state

let serializeLocation st = function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | OnStack i -> st.rspOffset - i |> sprintf "qword [rsp + %i]" //stackgrowsdown.com

let serializeInstruction st = 
  let serialize = serializeLocation st
  function 
  | Mov (l1,l2) -> sprintf "mov    %s, %s" (serialize l1) (serialize l2)
  | Add (l1,l2) -> sprintf "add    %s, %s" (serialize l1) (serialize l2)
  | Sub (l1,l2) -> sprintf "sub    %s, %s" (serialize l1) (serialize l2)
  | Cmp (l1,l2) -> sprintf "cmp    %s, %s" (serialize l1) (serialize l2)
  | Ret -> sprintf "ret"
  | Jnz l -> sprintf "jnz    %s" l
  | Syscall -> "syscall"
  | Push r -> sprintf "push %s" (serialize r)
  | Pop r -> sprintf "pop %s" (serialize r)


let addInstruction i = state{
  let! st = getState
  let  text = serializeInstruction st i
  do! putState {st with instructions = st.instructions + "\n        " + text}
}

let addComment s = updateState' (fun st -> {st with instructions = st.instructions + "        ;" + s})

let addLabel s = updateState' (fun st -> {st with instructions = st.instructions + s ":"})

let instruct = function
  | Push p as x -> assembly {
    do! updateState' (fun st -> {st with rspOffset = st.rspOffset + 8})
    do! addInstruction x }
  | Pop p as x -> assembly {
    do! updateState' (fun st -> {st with rspOffset = st.rspOffset - 8})
    do! addInstruction x }
  | x -> addInstruction x

let rec executeExpression = function
  | IntLit i -> Imm i |> returnM
  | StringLit s -> failf "No strings yet"
  | Variable v -> state {
     let! s = getState 
     return Map.find v s.variableLocations }
  | Func (n,[x;y]) when n = "Add" || n = "Sub" -> state {
      let instr = match n with | "Add" -> Add | "Sub" -> Sub | _ -> failf "Not supported function"

      let! xloc = executeExpression x 
      do! instruct <| Push xloc

      let! yloc = executeExpression y 
      do! instruct <| Mov (Reg RAX , yloc)
      
      do! instruct <| n [Mov (Reg RAX, xloc'); instr (Reg RAX, yloc)] , Reg RAX}
  | Func _ -> failf "Not supported function"


let instructs (_,e) = getInstructionsExpr e

let rec toInstructions = function
  | AReturnStat e-> state {
      let! (instrs, l) = e |> instructs 
      return instrs @ [Mov (Reg RDI, l); Mov (Reg RAX, Imm 60); Syscall]}
  | ADeclaration _ -> [] |> returnM
  | AAssignment (v, e) -> state {
      let! x = Map.find v.name << getVariables <!> getState
      let! (is, exprLoc) = e |> instructs 
      return is @ [Mov (getLocation x, exprLoc)]}
  | AExecution e -> e |> instructs |>> fst
  | AIfStat (n, g,b) -> state{
    let! (guardInstructs, guardLoc) = g |> instructs
    let! bodyinstructs = b |> mapM toInstructions |>> List.collect id
    return guardInstructs @ [Cmp (guardLoc, Imm 0); (Jnz n)] @ bodyinstructs @ [Label n]}

let funcToInstructions stats (varState : VarState) = 
  let rspOffset = varState.stackDepth + 8
  let init = [Sub (Reg RSP, Imm rspOffset)]
  let final = [Add (Reg RSP, Imm rspOffset)]
  let (body, state) = stats |> mapM toInstructions |>> List.collect id |> fun i -> run i varState
  init @ (body varState)
  |> List.map (serializeInstruction rspOffset)
  |> String.concat "\n"

let serializeModule fs = 
  match fs with
  | [] -> failf "no functions?"
  | x::y::xs -> failf "too many functions"
  | [x] -> let body = x ||> funcToInstructions 
           sprintf "        global _start\n \n        section .text \n_start:\n%s" body
*)