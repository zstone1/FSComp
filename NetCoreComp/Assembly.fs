module Assembly
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open InjectMoves
open AssignHomes
open ASTBuilder
open FSharpx.State
 
let serializeLocation (homes:Map<_,_>) = 
  let rspDepth = getVarStackDepth homes
  function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | Data s -> s
  | VarStack i -> 8 * (rspDepth - i) |> sprintf "qword [rsp + %i]" //stackgrowsdown.com

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



let intro sgn homes = [LabelA (sgn |> getCallLab);
                SubA (Reg RSP, Imm (getVarStackDepth homes))]
let outro sgn homes = [LabelA (sgn |> getEndLab);
                AddA (Reg RSP, Imm (getVarStackDepth homes));
                RetA]

let funcToInstructions sgn homes instrs = 
  intro sgn homes @ instrs@ outro sgn homes
  |> List.map (serializeInstruction homes)
  |> String.concat "\n"


let escapeString = function 
  | '\n' -> "\\n" 
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | c -> c.ToString()

let toDataLabel (lab, s) = sprintf "%s:\n        db     `%s`, 10, 0" lab (String.collect escapeString s)
let serializeModule {funcInstructions = fs; dataLits = l} = 
  let prgm = fs |> List.map funcToInstructions 
  let data = l |> List.map toDataLabel 
  let initialize = "        global main\n        extern printf\n        section .text"
  initialize :: prgm @ data |> String.concat "\n"
          
