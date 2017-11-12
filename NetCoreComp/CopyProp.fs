module CopyProp
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
open FSOption

(*
The idea of "copy propogation" is to coallesce instrucntions like 
x = y
z = 2 + x
into 
z = 2 + y
*)

type private CopyTraversal<'a>= {var: 'a; initialAssign : NodeId; coverage : NodeId list; value : 'a}

let private (|SingleSourceTraversal|_|) g t =
  match tryExactlyOne t.usedAssignments with
  | Some n -> SingleSourceTraversal (g.nodes.[n], t.witnessed) |> Some
  | None -> None

let private buildCopyTraversal n allTravs trav = function 
  | AssignI (v, VarAtom v') 
    -> let safeEdges = //Hack to protect function arguments
            allTravs 
            |> List.filter (fun i -> (i.liveVar = v') && (i.witnessed |> List.exists ( snd >> ((=) n) )))
            |> List.collect (fun i -> i.witnessed)
       let coverage = safeEdges |> intersect <| trav.witnessed
       {initialAssign = n; var = v; value = v'; coverage = coverage |> List.map snd} |> Some
  | _ -> None

///Gets the coressponding CopyTraversal for @trav, if the LivenessTraversal has a single source.
let private getCopyTraversals (g : Graph<_,_>) allTravs trav = 
  match trav.usedAssignments with
  | [n] -> buildCopyTraversal n allTravs trav (g.nodes.[n].instruction)  
  | _ -> None

///This returns (NodeId -> instruct<'a>)'s that are not valid
///adjacency maps because some assigments might be pruned.
///However the order is preserved, so another graph can be constructed
let private propogate g copyTrav = 
  let propCopy key node =
    if (copyTrav.coverage |> List.contains key)
    then 

        let replace i = if copyTrav.var = i
                        then copyTrav.value
                        else i
        let replaceAt = function 
          | VarAtom x -> x |> replace |> VarAtom
          | x -> x
        let newInstr =
          match node.instruction with
          | AddI (a,b) -> AddI (a, replaceAt b)
          | CmpI (a,b) -> CmpI (replace a, replaceAt b)
          | SubI (a,b) -> SubI (a, replaceAt b)
          | IMulI (a,b) -> IMulI (a, replaceAt b)
          | AssignI (a,b) -> AssignI (a, replaceAt b)
          | JmpI (l) -> JmpI (l)
          | JzI (l) -> JzI (l)
          | CallI (v,l,args) -> CallI (v, l, List.map replace args)
          | LabelI (l) -> LabelI (l)
          | JnzI l -> JnzI l
          | ReturnI (v) -> ReturnI (replace v)
          | SeteI v -> SeteI v
        {node with instruction = newInstr}
     
    else node
  g |> Map.map propCopy

let private propogateAllCopies il = 
  let g = il |> toGraph
  let allTravs = g |> getTraversals |> Seq.toList
  let travs = allTravs
           |> Seq.choose (getCopyTraversals g allTravs)
  
  let newG = travs |> Seq.fold propogate g.nodes |> mapValues |> List.map (fun i -> i.instruction)
  let x = newG.Length
  newG

let rec private propogateUntilDone' lastTime il = 
  if lastTime = il
  then il 
  else propogateUntilDone' il (propogateAllCopies il)

let private propogateUntilDone il = propogateUntilDone' [] il
let propogateCopies m = {m with funcs = m.funcs |> List.map (snd_set propogateUntilDone)}
