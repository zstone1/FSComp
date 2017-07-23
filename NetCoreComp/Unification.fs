module Unification
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
///Given a node where variable v is read,
///This returns all nodes where v is live
///up to readNode.
let rec private trackParents' v (adj:Map<_,_>) (g:Map<_,_>) n = state {
  let! s = getState
  match List.contains n s with
  | true -> return ()
  | false -> 
      do! (n :: s) |> putState 
      let writtenVars = g.[n].instruction |> getWrittenVariables
      match List.contains v writtenVars with
      | true -> return ()
      | false ->
         let parents = adj.[n].ins
         //non-tail recursive... probably bad.
         do! mapU (trackParents' v adj g) parents
}        

let private trackParents v adj g n = exec (trackParents' v adj g n) []

let private getLiveNodes graph adj = query {
  for (k,c) in Map.toSeq graph do
  for var in getReadVariables c.instruction do
  for l in  trackParents var adj graph k do
  select (var, l)
}

let private toLivnessGraph (s:seq<_>) = query {
  for (v1, n1) in s do
  join (v2, n2) in s on (n1 = n2)
  select (v1, v2) 
  distinct into x
//  where (fst x <> snd x)
  groupBy (fst x) into g 
  select (g.Key, Seq.map snd g)
}

let private assignColor color node = updateStateU <| addOrUpdate node color (konst color)

let private greedyColorGraph nextColor (g:Map<_,_>) =
 let evaled = (Map.map (konst List.ofSeq) g)
 state {
  for x in g do 
    let! s = getState
    let n = Seq.choose (Map.tryFind |> flip <| s)  x.Value
    let c = nextColor n
    do! assignColor c x.Key
}

let private pickNext cs = Seq.fold max 0 cs |> (+) 1
  
let private colorGraph a = greedyColorGraph pickNext a

let private addFunctionArgNode args g = 
  let nodeId = -1
  let addArg {ASTVariable.name = n} = (VarName n, nodeId)
  let argNodes = args |> Seq.map addArg 
  argNodes |> Seq.append g

let private colorIL args = 
     toGraph
  >> (getLiveNodes |> uncurry)
  >> addFunctionArgNode args
  >> toLivnessGraph
  >> Map.ofSeq
  >> colorGraph
  >> (exec |> flip <| Map.empty)

let private unifiedName i = sprintf "_unified_%i" i
let private replaceVar (coloring:Map<_,_>) v = 
  coloring.[v] |> unifiedName |> VarName
let private replaceAtom coloring = function 
  | VarAtom v -> replaceVar coloring v |> VarAtom
  | x -> x

let private replaceAstVar (coloring :Map<_,_>) (v:ASTVariable) = 
  let newName = Map.tryFind (VarName v.name) coloring 
             |> Option.map unifiedName
             |> Option.defaultValue v.name
  {v with name = newName}

let private unifyVars c = 
  mapInstruct 
    (replaceVar c) 
    (replaceVar c |> Option.map)
    (replaceAtom c) 
    id

let private unifyVariables (signature, il) = 
  let coloring = colorIL signature.args il
  let newIl = il |> List.map (unifyVars coloring)
  let newSig = {signature with args = List.map (replaceAstVar coloring) signature.args}
  (newSig, newIl)

let unifyModule (m : FlattenedModule) = 
  {m with funcs = List.map unifyVariables m.funcs}

 
