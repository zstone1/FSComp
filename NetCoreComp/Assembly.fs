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
 
let getVarStackDepth homes =
  Seq.sumBy (snd >> function
    | Stack(VarStack i,_) -> 1
    | _ -> 0) (homes 
  |> Map.toSeq)

let getSavedVariableDepth homes =
  homes 
  |> (getSaveRegs |> flip) calleeSave
  |> List.length

let getAlignmentAdjust homes = 
  let size = getVarStackDepth homes
           + getSavedVariableDepth homes
  size % 2

let rec serializeLocation (homes: Homes<_>) = 
  let homeDepth = getVarStackDepth homes + getSavedVariableDepth homes + getAlignmentAdjust homes
  function 
  | Reg x -> (sprintf "%A" x).ToLowerInvariant()
  | Imm (i) -> i.ToString()
  | Data s -> s
  | Stack(VarStack i, o) 
    -> (match o with | FromHome s -> homeDepth + s )
    |> fun x -> x - (i + 1) 
    |> (*) 8
//    |> (+) (match o with FromHome)
    |> sprintf "qword [rsp + %i]" //stackgrowsdown.com
  | Stack(PreStack i,o) 
    -> (match o with | FromHome s -> homeDepth + s )
    |> (+) i
    |> (+) 1 //rtn ptr
    |> (*) 8
    |> sprintf "qword [rsp + %i]" 
  | Stack(PostStack _,_) -> failf "Should not reference postStack this far"

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
  | XchgA (l1,l2) -> handleOp2 "xchg" l1 l2
  | Nop -> ""


let introOutroAdj homes = getVarStackDepth homes + getAlignmentAdjust homes
let intro sgn homes = 
  LabelA (sgn |> getCallLab)
  :: saveCalleeRegs homes
  (*@ [ SubA (Reg RSP, Imm ( 8 * introOutroAdj homes))]*)
let outro sgn homes =
  [
    LabelA (sgn |> getEndLab); 
    AddA (Reg RSP, Imm (8 * introOutroAdj homes));
  ]
  (*
  @ restoreCalleeRegs homes
  @ [RetA] *)

let funcToInstructions (sgn, homes, instrs) = 
  intro sgn homes @ instrs@ outro sgn homes
  |> List.map (serializeInstruction homes)
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
          
