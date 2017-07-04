module Assembly
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open Assignment
open ASTBuilder
open FSharpx.State
 
type AssemblyState = {
  rspOffset : int
}

let getDepthWithOffset st = st.stackDepth + (st.stackDepth - 8) % 16

let serializeLocation stackDepth = function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | Stack { distFromBase = b; currentRspMod = modifier} ->
       (stackDepth + modifier) - b |> sprintf "qword [rsp + %i]" //stackgrowsdown.com

let serializeInstruction st = 
  let serialize = serializeLocation st
  let handleOp2 s l1 l2 = sprintf "        %s    %s, %s" s (serialize l1) (serialize l2)
  let handleOp1Loc s l1 = sprintf "        %s    %s" s (serialize l1)
  let handleOp1 s l1 = sprintf "        %s    %s" s l1 
  let handleOp0 s = sprintf "        %s" s
  function 
  | MovA (l1,l2) -> handleOp2 "mov" l1 l2
  | AddA (l1,l2) -> handleOp2 "add" l1 l2
  | SubA (l1,l2) -> handleOp2 "sub" l1 l2
  | CmpA (l1,l2) -> handleOp2 "cmp" l1 l2
  | JnzA (LabelName l) -> handleOp1 "jnz" l
  | JmpA (LabelName l) -> handleOp1 "jmp" l
  | SyscallA -> handleOp0 "syscall"
  | LabelA (LabelName l) -> sprintf "%s:" l
  | CallA (LabelName l) -> handleOp1 "call" l 
  | RetA -> handleOp0 "ret"
  | PushA l -> handleOp1Loc "push" (Reg l)
  | PopA l -> handleOp1Loc "pop" (Reg l)


let intro st = [LabelA (st.callLabName |> LabelName);
                SubA (Reg RSP, Imm (getDepthWithOffset st))]
let outro st = [LabelA (st.rtnLabName |> LabelName);
                AddA (Reg RSP, Imm (getDepthWithOffset st));
                RetA]

let funcToInstructions (s: ASTSignature, st : AssignSt) = 
  intro st @ st.ainstructs @ outro st 
  |> List.map (serializeInstruction (getDepthWithOffset st))
  |> String.concat "\n"

let handlerBody = 
  [
    LabelA ("_start" |> LabelName)
    SubA (Reg RSP, Imm 8)
    CallA ("main" |> LabelName)
    AddA (Reg RSP, Imm 8)
    MovA (Reg RDI, Reg RAX)
    MovA (Reg RAX, Imm 60)
    SyscallA
  ]

let serializeModule fs = 

  match fs with
  | [] -> failf "no functions?"
  | xs -> let prgm = List.map funcToInstructions xs
          let handler = List.map (serializeInstruction 0) handlerBody
          let initialize = "        global _start\n        section .text"
          initialize :: handler @ prgm |> String.concat "\n"
          
