module PruneDeadCode
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang


let private isUsedIL usedIds nodeId = function  
  | {instruction = AssignI (ILVarName _, _)} -> usedIds |> Seq.contains nodeId 
  | _ -> true

let private isUsedML usedIds nodeId = function  
  | {instruction = AssignI (MLVarName _, _)} -> usedIds |> Seq.contains nodeId 
  | _ -> true

let private pruneUnusedAssignments isUsed ml =
  //Note that it's critical that everything here retains order.
  let graph = ml |> toGraph
  let traverals = graph |> getTraversals
  let usedIds = traverals |> Seq.collect (fun i -> i.usedAssignments)
  let pruned = graph.nodes 
            |> Map.filter (isUsed usedIds)
            |> Map.map (fun k v -> v.instruction)
  pruned |> mapValues

let rec private pruneMlUntilDone' isUsed lastOne ml = 
  if lastOne = List.length ml
  then ml 
  else pruneMlUntilDone' isUsed (List.length ml) (pruneUnusedAssignments isUsed ml)

let pruneMlUntilDone isUsed = pruneMlUntilDone' isUsed -1

let pruneDeadBranchesIL m = {
    lits = m.lits
    funcs = m.funcs |>List.map (snd_set (pruneMlUntilDone isUsedIL))
}

let pruneDeadBranchesML m = {
    lits = m.lits
    funcs = m.funcs |>List.map (snd_set (pruneMlUntilDone isUsedML))
}