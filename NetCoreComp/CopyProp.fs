module CopyProp
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
open FSOption


type CopyTraversal<'a>= {var: 'a; initialAssign : NodeId; coverage : NodeId list; value : Atom<'a>}
let getProp g n allTravs trav =
  let result v a c= {initialAssign = n; var = v; value = a; coverage = c} |> Some
  function 
  | AssignI (v, (IntLitAtom _ as a)) | AssignI (v, (DataRefAtom _ as a)) 
    -> result v a (trav.witnessed |> List.map snd)
  | AssignI (v, VarAtom v') 
    -> let k = allTravs 
            |> List.filter (fun i -> (i.liveVar = v') 
                                  && (i.witnessed |> List.exists ( snd >> ((=) n) )))
            |> List.maxBy (fun i -> i.witnessed.Length)
       let coverage = k.witnessed |> intersect trav.witnessed |> List.map snd
       result v (VarAtom v') coverage
  | _ -> None

let getCopyTraversals (g : Graph<_,_>) allTravs trav =  maybe {
  let! n = tryExactlyOne trav.usedAssignments
  return! g.nodes.[n].instruction |> getProp g n allTravs trav 
}

///This returns NodeId -> instruct<'a>'s that are not valid graphs,
///because const assignments are pruned. 
let private propogate g constTrav = 
  let propConst key node =
    if (constTrav.coverage |> List.contains key)
    then 
        let newInstr =
          mapInstruct
            id
            (fun i -> if (VarAtom constTrav.var) = i
                      then constTrav.value
                      else i)
            id
            node.instruction
        {node with instruction = newInstr}
     
    else node
  g |> Map.map propConst

let propogateAllConstants il = 
  let g = il |> toGraph
  let allTravs = g |> getTraversals |> Seq.toList
  let travs = allTravs
           |> Seq.choose (getCopyTraversals g allTravs)
  
  let newG = travs |> Seq.fold propogate g.nodes |> mapValues |> List.map (fun i -> i.instruction)
  let x = newG.Length
  newG

let rec propogateUntilDone' lastTime il = 
  if lastTime = il
  then il 
  else propogateUntilDone' il (propogateAllConstants il)

let propogateUntilDone il = propogateUntilDone' [] il
let propogateConstants m = {m with funcs = m.funcs |> List.map (snd_set propogateUntilDone)}
