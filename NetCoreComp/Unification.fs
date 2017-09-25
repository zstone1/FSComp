module Unification
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang
///Given a node where variable v is read,
///This returns all nodes where v is live
///up to readNode.

type StackLoc = 
  | PreStack of int
  | VarStack of int
  | PostStack //PostStack sucks, and is handled by carefully keeping track of the order of operations.

type Location = 
  | Reg of Register
  | Data of string
  | Imm of int
  | Stack of StackLoc 

let private replaceVar' (coloring:Map<_,_>) = function
  | MLVarName _ as v -> coloring |> Map.tryFind v
  | (RegVar r) -> Reg r |> Some
  | (IncomingStack i) -> Stack (PreStack i) |> Some
  | OutgoingStack -> Stack PostStack |> Some

let private replaceVar (coloring:Map<_,_>) = (replaceVar' coloring) >> Option.get

type LivenessTraversal = {witnessed : (NodeId * NodeId) list; usedAssignments : NodeId List}
///Traverses up the computation graph, starting at @n, to determine all of the nodes where @v is alive (assuming it is alive at below or in @n) 
let rec private trackParents' (v  ) (compGraph : DiGraph<_>) n = state {
  let! s = getState
  let completedLoop = List.contains n (s.witnessed |> List.map snd)
  let parents = compGraph.adj.[n].ins
  let parentEdges = parents  |> List.map (fun i -> (i,n))
  let writtenVars = compGraph.nodes.[n].instruction |> getWrittenVariables

  match writtenVars with
  | Some v' when v' = v 
    -> do! (fun st -> { st with usedAssignments = n :: s.usedAssignments}) |> updateStateU
  | Some _ | None 
    -> do! (fun st -> { st with witnessed = parentEdges @ st.witnessed}) |> updateStateU
       if completedLoop
         then return ()
         else do! mapU (trackParents' v compGraph) parents
}        

let private trackParents v compGraph n = exec (trackParents' v compGraph n) {witnessed = []; usedAssignments = []}

///Returns a pair of Variable->int for every node where 
///the variable is live.
let private getTraversals compGraph = query {
  for (k,c) in Map.toSeq compGraph.nodes do
  //TODO: optimize away writes to variables that are never read from.
  for var in getReadVariables c.instruction do
  let result = trackParents var compGraph k 
  select (var, result)
}

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

let private assignColor (color) (node : MixedVar) = state {
   do! updateStateU <| addOrUpdate node color (konst color)
   return color
}

let private validateColoring (adjNodes) thisColor = state {
  let! coloringSoFar = getState
  let colors = adjNodes |> List.choose (Map.tryFind|>flip<| coloringSoFar)
  if colors |> List.contains thisColor 
  then failComp "Graph coloring failed due to bad fixed colors"
  else return ()
}

///Retains color for those variables that are in fixed positions
///Otherwise use @pickAndAssignColor
let private colorGraph pickAndAssignColor (g: Map<_,_>) = state {
  let sorted = List.sortBy (fst >> function 
    | RegVar _ | IncomingStack _ | OutgoingStack _ -> 0
    | _ -> 1)
  for (key, adjs) in sorted (g |> Map.toList ) do
    let! newColor = 
      match key with
      | (RegVar r)  -> key |> assignColor (Reg r) 
      | (IncomingStack i)  -> key |> assignColor (Stack (PreStack i)) 
      | OutgoingStack -> key |> assignColor (Stack PostStack)
      | _ -> pickAndAssignColor key adjs
    //This makes assumptions about the ml being valid,
    //in the sense of two fixed variables won't be in the same 
    //place at the same time. This will fail fast if it's not valid.
    //It can always be patched by adding more temp variables.
    do! validateColoring g.[key] newColor
}

let regColors = (List.map Reg [RDI;RSI;RDX;RCX;R8;R9;R15; R14; R13; R12; RBP; RBX; RAX])
let stackColors = (Seq.initInfinite (Stack << VarStack) )
let colors = Seq.append regColors stackColors
               
let greedyNextColor cs = Seq.find (fun c -> not <| Seq.contains c cs) colors

let private pickAndAssignGreedy key adjs = state {
  let! coloringSoFar = getState
  let disallowed = adjs |> Seq.choose (Map.tryFind |> flip <| coloringSoFar)
  let c = greedyNextColor disallowed
  return! assignColor c key
}

let private pickAndAssignAfineGreedy (afinity:Map<Atom<_>, Atom<_> list>) key adjs  = state {
  let! coloringSoFar = getState
  let friends = afinity.[key |> VarAtom] 
             |> List.collect (fun i -> afinity.[i])
             |> List.collect (fun i -> afinity.[i])
             |> List.distinct
  let afinities = friends
               |> List.choose (function 
                  | VarAtom v -> replaceVar' coloringSoFar v
                  | DataRefAtom _ | IntLitAtom _ -> None)

  let disallowed = adjs |> Seq.choose (Map.tryFind |>flip<| coloringSoFar)
  match afinities |> List.filter ((Seq.contains |>flip<| disallowed) >> not) with
  | [] -> return! assignColor (greedyNextColor disallowed) key
  | x::xs -> return! assignColor x key
}
  
let private colorGraphGreedy inter afinities = colorGraph pickAndAssignGreedy inter

let private colorGraphAfineGreedy inter affinites = colorGraph (pickAndAssignAfineGreedy affinites) inter

let private pruneUnusedAssignments ml =
  //Note that it's critical that everything here retains order.
  let graph = ml |> toGraph
  let traverals = graph |> getTraversals
  let usedIds = traverals |> Seq.collect (fun (_,i) -> i.usedAssignments)
  let isUsed nodeId = function  
    | {instruction = AssignI (MLVarName _, _)} -> usedIds |> Seq.contains nodeId 
    | _ -> true
  let pruned = graph.nodes 
            |> Map.filter isUsed 
            |> Map.map (fun k v -> v.instruction)
  pruned |> mapValues

let rec private pruneMlUntilDone' lastOne ml = 
  if lastOne = List.length ml
  then ml 
  else pruneMlUntilDone' (List.length ml) (pruneUnusedAssignments ml)

let private pruneMlUntilDone x = pruneMlUntilDone' -1 x

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

let private unifyVars c = mapInstructBasic (replaceVar c)

type UnifiedSignature = CompSignature<Location>

let private unifyVariables colorAlgo (signature : MixedSignature , ml) = 
  let pruned = pruneMlUntilDone ml
  let coloring= colorML colorAlgo pruned
  let newIl = pruned |> List.map (unifyVars coloring)
  let newSig = 
    {
      UnifiedSignature.args = List.map (replaceVar coloring) signature.args
      name = signature.name
      returnTy = signature.returnTy
    }
  (newSig, newIl)

type UnifiedModule = CompModule<Location>
let unifyModule (m : MLModule) = 
  let coloring = match globalSettings.allocation with  
                 | RegGreedy -> colorGraphGreedy
                 | AffineGreedy -> colorGraphAfineGreedy
                 | _ -> colorGraphGreedy
  {
    UnifiedModule.lits = m.lits
    UnifiedModule.funcs = List.map (unifyVariables coloring) m.funcs
  }

 
