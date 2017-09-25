module ConstantProp
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
open FSOption


type ConstTraversal<'a>= {var: 'a; initialAssign : NodeId; coverage : NodeId list; value : int}
let getConstantTraversal (g :Map<_,_>) trav =  maybe {
  let! n = tryExactlyOne trav.usedAssignments
  let! (v, c) = match g.[n].instruction with
                | AssignI (v, IntLitAtom c) -> (v,c) |> Some
                | _ -> None
  let coverage = trav.witnessed |> List.map snd
  let coveredInstrs = [for n in coverage do yield g.[n].instruction]
  for instr in coveredInstrs do 
    do! match getExposedVar instr with
        | Some v' when v = v' -> None
        | _ -> Some ()
  return {
      initialAssign = n
      var = v
      value = c
      coverage = coverage
  }
}

///This returns NodeId -> instruct<'a>'s that are not valid graphs,
///because const assignments are pruned. 
let private propogate g constTrav = 
  let pruned = g |> Map.remove constTrav.initialAssign
  let propConst key node =
    if (constTrav.coverage |> List.contains key)
    then 
        //TODO: if the variable ever appears outside an atom, do not propogate.
        let newInstr =
          mapInstruct
            id
            (Option.map id)
            (fun i -> if (VarAtom constTrav.var) = i
                      then IntLitAtom constTrav.value
                      else i)
            id
            node.instruction
        {node with instruction = newInstr}
     
    else node
  pruned |> Map.map propConst

let propogateAllConstants il = 
  let g = il |> toGraph
  let travs = g 
           |> getTraversals 
           |> Seq.map snd 
           |> Seq.choose (getConstantTraversal g.nodes)
  
  travs |> Seq.fold propogate g.nodes |> mapValues |> List.map (fun i -> i.instruction)


let propogateConstantsInModule m = {m with funcs = m.funcs |> List.map (snd_set propogateAllConstants)}
