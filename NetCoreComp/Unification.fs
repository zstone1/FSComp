module Unification
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
open GraphColorings

///Given the list of variable -> live node pairs,
///builds joins against itself to produce the 
///adjacency map of the liveness graph.
let private toInterferenceGraph (s:seq<_>) = query {
  for (v1, n1) in s do
  join (v2, n2) in s on (n1 = n2)
  select (v1, v2) 
  distinct into x
  groupBy (fst x) into g 
  select (g.Key, g |> Seq.map snd |> Seq.filter (fun v' -> v' <> g.Key))
}


               
let computeAffinity = function 
  | AssignI (x,y) -> [(VarAtom x, y); (y, VarAtom x)]
  | _ -> []

let allAffinities ml = 
  query {
    for (a,b) in ml |> List.collect computeAffinity do
    groupValBy b a into g
    select (g.Key, g |> List.ofSeq) }
  |> Map.ofSeq

let callerSave = [RAX; RDI; RSI; RDX; RCX; R8; R9; R10; R11;] 

let getInterferenceMap ml = seq {
  let graph = ml |> toGraph

  for (v, trav) in graph |> getTraversals do
    for edge in trav.witnessed do 
      yield (v, edge)
  
  for x in graph.nodes do
    match x.Value with
    | {instruction = CallI _; next = Step q; id = p} 
      -> yield! callerSave |> Seq.map (RegVar >> fun i -> (i, (p,q)))
    | _ -> yield! []
}

let private colorML coloring x = 
  let interference = x 
                  |> getInterferenceMap 
                  |> toInterferenceGraph
                  |> Seq.map (snd_set List.ofSeq)
                  |> Map.ofSeq
  let afinity = allAffinities x
  coloring interference afinity |>exec<| Map.empty

let private replaceVar (coloring:Map<_,_>) = (replaceVar' coloring) >> Option.get

let private unifyVars c = mapInstructBasic (replaceVar c)

type UnifiedSignature = CompSignature<Location>

let private unifyVariablesInFunc colorAlgo (signature: CompSignature<_>, ml) = 
  let coloring= colorML colorAlgo ml
  let newIl = ml |> List.map (unifyVars coloring)
  let newSig = 
    {
      UnifiedSignature.args = List.map (replaceVar coloring) signature.args
      name = signature.name
      returnTy = signature.returnTy
    }
  (newSig, newIl)

type UnifiedModule = CompModule<Location>
let unifyVariables allocType (m : MLModule) = 
  let coloring = match allocType with  
                 | RegGreedy -> colorGraphGreedy
                 | AffineGreedy -> colorGraphAfineGreedy
                 | StackOnly -> colorGraphStackOnly
  {
    UnifiedModule.lits = m.lits
    UnifiedModule.funcs = m.funcs |> List.map (unifyVariablesInFunc coloring) 
  }

 
