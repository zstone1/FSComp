module Assembly
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open Assignment
open FSharpx.State
 
type AssemblyState = {
  rspOffset : int
}

let serializeLocation st = function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | Stack i -> st.rspOffset - i |> sprintf "qword [rsp + %i]" //stackgrowsdown.com

let serializeInstruction st = 
  let serialize = serializeLocation st
  let handleOp s l1 l2 = sprintf "        %s    %s, %s" s (serialize l1) (serialize l2)
  function 
  | MovA (l1,l2) -> handleOp "mov" l1 l2
  | AddA (l1,l2) -> handleOp "add" l1 l2
  | SubA (l1,l2) -> handleOp "sub" l1 l2
  | CmpA (l1,l2) -> handleOp "cmp" l1 l2
  | JnzA (LabelName l) -> sprintf "        jnz    %s" l
  | SyscallA -> "        syscall"
  | LabelA (LabelName l) -> sprintf "%s:" l

let funcToInstructions (st : AssignSt) = 
  let asmState = {rspOffset = st.stackDepth}
  let init = [SubA (Reg RSP, Imm asmState.rspOffset)]
  let final = [Add (Reg RSP) (Imm asmState.rspOffset)]
  (init @ st.ainstructs)
  |> List.map (serializeInstruction asmState)
  |> String.concat "\n"


let serializeModule fs = 
  match fs with
  | [] -> failf "no functions?"
  | x::y::xs -> failf "too many functions"
  | [x] -> let body = funcToInstructions x
           sprintf "        global _start\n \n        section .text \n_start:\n%s" body