module AssignReg
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open FSharpx.State

type Register = 
  | RAX
  | RDI
  | RSP
  | RSI
  | RDX
  | RCX
  | R8
  | R9

type Location = 
  | Reg of Register
  | Data of string
  | Stack of int
  | Imm of int

type Assignments = Map<Location, Variable>

type AssignNode = {
  id : int
  instruction : Instruct
  next : Next<int>
  assignments : Assignments
}

let callingRegs = [RDI;RSI; RDX;RCX; R8; R9] 
let private initialAssignments {ASTSignature.args = SplitAt 6 (l,r)} = 
  let regArgs = Seq.zip (List.map Reg callingRegs) (List.map toVar l)
  let stackArgs = r
               |> List.map toVar
               |> List.indexed
               |> List.map (fun (i,j) -> (Stack -(i+1), j))
  regArgs |> Seq.append stackArgs |> Map.ofSeq


let private incomingRequirements = function
  | CallI (_, _, (SplitAt 6 (l,r))) -> []
  | _ -> []

(*

let passArgsByConvention (SplitAt 6 (l, r)) = state {
    let! regPass = l 
                |> mapM (fun i -> getLoc i <!> getState)
               |>> Seq.zip (List.map Reg callingRegs)
               |>> Seq.map MovA
               |>> Seq.toList
    yield! regPass
    let putArgOnStack l = state {
      let! loc = getLoc l <!> getState
      yield MovA (Reg RAX, loc)
      do! modifyRsp 8
      yield PushA RAX
    }               
    do! r |> List.rev |> mapU putArgOnStack
    yield MovA (Reg RAX, Imm 0)
    return state {
      do! modifyRsp (-8 * r.Length)
      yield AddA (Reg RSP, ( Imm (8*r.Length)))
      }
  }*)