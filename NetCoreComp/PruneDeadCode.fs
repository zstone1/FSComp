module PruneDeadCode
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang


let private pruneUnusedAssignments ml =
  //Note that it's critical that everything here retains order.
  let graph = ml |> toGraph
  let traverals = graph |> getTraversals
  let usedIds = traverals |> Seq.collect (fun (_,i) -> i.usedAssignments)
  let isUsed nodeId = function  
    | {instruction = AssignI (ILVarName _, _)} -> usedIds |> Seq.contains nodeId 
    | _ -> true
  let pruned = graph.nodes 
            |> Map.filter isUsed 
            |> Map.map (fun k v -> v.instruction)
  pruned |> mapValues

let rec private pruneMlUntilDone' lastOne ml = 
  if lastOne = List.length ml
  then ml 
  else pruneMlUntilDone' (List.length ml) (pruneUnusedAssignments ml)

let pruneMlUntilDone x = pruneMlUntilDone' -1 x

let pruneDeadBranches m = {
    lits = m.lits
    funcs = m.funcs |>List.map (snd_set pruneMlUntilDone)
}