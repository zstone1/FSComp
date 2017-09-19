module Assembly
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open InjectMoves
open AssignHomes
open ASTBuilder
open FSharpx.State
open MixedLang
open Unification
 
//Stack is organized like |stack args| variable homes | callee save | alignment | callerSave | stack args
//                                    ^
//                                 adjustment is difference to here.
let serializeLocation adjustment = function
  | Imm i -> i.ToString()
  | Reg r -> (sprintf "%A" r).ToLowerInvariant()
  | Data s -> s
  | Stack (VarStack i)
    -> sprintf "qword [rsp + %i]" ((adjustment - i) * 8)
  | Stack (PreStack i)
    -> sprintf "qword [rsp + %i]" ((adjustment + i) * 8)
  | Stack (PostStack _) 
    -> failComp "Never write to poststack. Only push/pop"

let updateAdjustment i = state {
  let! a = getState
  do! putState (a+i)
}
let serializeInstruction instr = state {
  let! serialize = serializeLocation <!> getState
  let handleOp2 s l1 l2 = sprintf "        %s    %s, %s" s (serialize l1) (serialize l2)
  let handleOp1Loc s l1 = sprintf "        %s    %s" s (serialize l1)
  let handleOp1 s l1 = sprintf "        %s    %s" s l1 
  let handleOp0 s = sprintf "        %s" s
  let str =  
    match instr with 
    | MovA (l1,l2) -> handleOp2 "mov" l1 l2
    | AddA (l1,l2) -> handleOp2 "add" l1 l2
    | SubA (l1,l2) -> handleOp2 "sub" l1 l2
    | IMulA (l1,l2) -> handleOp2 "imul" l1 l2
    | CmpA (l1,l2) -> handleOp2 "cmp" l1 l2
    | JnzA (LabelName l) -> handleOp1 "jnz" l
    | JmpA (LabelName l) -> handleOp1 "jmp" l
    | SyscallA -> handleOp0 "syscall"
    | LabelA (LabelName l) -> sprintf "%s:" l
    | CallA (LabelName l) -> handleOp1 "call" l 
    | RetA -> handleOp0 "ret"
    | PushA l -> handleOp1Loc "push" (Reg l)
    | PopA l -> handleOp1Loc "pop" (Reg l)
    | Nop -> ""
  match instr with 
  | AddA (Reg RSP, Imm i) -> do! updateAdjustment i
  | SubA (Reg RSP, Imm i) -> do! updateAdjustment -i
  | PushA _ -> do! updateAdjustment -1
  | PopA _ -> do! updateAdjustment 1
  | _ -> return ()

  return str
}

let funcToInstructions (instrs) = 
  instrs
  |> mapM (serializeInstruction)
  |> (eval |> flip <| 0)
  |> String.concat "\n"


let escapeString = function 
  | '\n' -> "\\n" 
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | c -> c.ToString()

let toDataLabel (lab, s) = sprintf "%s:\n        db     `%s`, 10, 0" lab (String.collect escapeString s)
let serializeModule {AsmModule.funcs = fs; lits = l} = 
  let prgm = fs |> List.map funcToInstructions
  let data = l |> List.map toDataLabel 
  let initialize = "        global main\n        extern printf\n        section .text"
  initialize :: prgm @ data |> Seq.filter ((<>)"") |> String.concat "\n"
          
