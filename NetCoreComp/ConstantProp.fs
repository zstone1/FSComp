module ConstantProp
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
open FSOption


type private ConstTraversal<'a>= {var: 'a; initialAssign : NodeId; coverage : NodeId list; value : int}
let private getConstantTraversal (g :Map<_,_>) trav =  maybe {
  let! n = tryExactlyOne trav.usedAssignments
  let! (v, c) = match g.[n].instruction with
                | AssignI (v, IntLitAtom c) -> (v,c) |> Some
                | _ -> None
  let coverage = trav.witnessed |> List.map snd
  return {
      initialAssign = n
      var = v
      value = c
      coverage = coverage
  }
}

///This returns (NodeId -> instruct<'a>)'s that are not valid
///adjacency maps because const assignments are pruned. But order is preserved
let private propogate g constTrav = 
  let propConst key node =
    if (constTrav.coverage |> List.contains key)
    then 
        let newInstr =
          mapInstruct
            id
            (fun i -> if (VarAtom constTrav.var) = i
                      then IntLitAtom constTrav.value
                      else i)
            id
            node.instruction
        {node with instruction = newInstr}
     
    else node
  g |> Map.map propConst

let private propogateAllConstants il = 
  let g = il |> toGraph
  let travs = g 
           |> getTraversals 
           |> Seq.choose (getConstantTraversal g.nodes)
  
  let newG = travs |> Seq.fold propogate g.nodes |> mapValues |> List.map (fun i -> i.instruction)
  let x = newG.Length
  newG

let rec private propogateUntilDone' lastTime il = 
  if lastTime = il
  then il 
  else propogateUntilDone' il (propogateAllConstants il)

let private propogateUntilDone il = propogateUntilDone' [] il
let propogateConstants m = {m with funcs = m.funcs |> List.map (snd_set propogateUntilDone)}
