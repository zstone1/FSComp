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
  | PostStack of int

type Location = 
  | Reg of Register
  | Data of string
  | Imm of int
  | Stack of StackLoc 

///Traverses up the computation graph, starting at @n, to determine all of the nodes where @v is alive (assuming it is alive at @n) 
let rec private trackParents' (v : MixedVar) (compGraph : DiGraph<_>) n = state {
  let! s = getState
  let completedLoop = List.contains n s
  match completedLoop with
  | true -> return () 
  | false -> 
      do! (n :: s) |> putState 
      let writtenVars = compGraph.nodes.[n].instruction |> getWrittenVariables
      let deadAbove = List.contains v writtenVars 
      match deadAbove with
      | true -> return () 
      | false ->
         let parents = compGraph.adj.[n].ins
         //non-tail recursive... probably bad.
         do! mapU (trackParents' v compGraph) parents 
}        

let private trackParents v compGraph n = exec (trackParents' v compGraph n) []

///Returns a pair of Variable->int for every node where 
///the variable is live.
let private getLiveNodes compGraph = query {
  for (k,c) in Map.toSeq compGraph.nodes do
  for var in getReadVariables c.instruction do
  for l in  trackParents var compGraph k do
  select (var, l)
}

///Given the list of variable -> live node pairs,
///builds joins against itself to produce the 
///adjacency map of the liveness graph.
let private toLivnessGraph (s:seq<_>) = query {
  for (v1, n1) in s do
  join (v2, n2) in s on (n1 = n2)
  select (v1, v2) 
  distinct into x
  groupBy (fst x) into g 
  select (g.Key, Seq.map snd g)
}

let private assignColor (color) (node : MixedVar) = state {
   do! updateStateU <| addOrUpdate node color (konst color)
   return color
}

let private validateColoring (adjNodes) thisColor = state {
  let! coloringSoFar = getState
  let colors = adjNodes |> List.map (Map.find|>flip<| coloringSoFar)
  if colors |> List.contains thisColor 
  then failComp "Graph coloring failed due to bad fixed colors"
  else return ()
}

///Retains color for those variables that are in fixed positions
///Otherwise use @pickAndAssignColor
let private colorGraph pickAndAssignColor (g: Map<_,_>) = state {
  for x in g do
    let! newColor = 
      match x.Key with
      | (RegVar r)  -> x.Key |> assignColor (Reg r)
      | (IncomingArg i)  -> x.Key |> assignColor (Stack (PreStack i))
      | (StackArg i) ->  x.Key |> assignColor (Stack (PostStack i))
      | _ -> pickAndAssignColor x
    //This makes assumptions about the ml being valid,
    //in the sense of two fixed variables won't be in the same 
    //place at the same time. This will fail fast if it's not valid.
    //It can always be patched by adding more temp variables.
    do! validateColoring g.[x.Key] newColor
}

let colors = Seq.append 
               (List.map Reg [RDI;RSI;RDX;RCX;R8;R9;R15; R14; R13; R12; RBP; RBX; RAX])
               (Seq.initInfinite (Stack << VarStack) )

let greedyNextColor cs = Seq.find (fun c -> not <| Seq.contains c cs) colors

let private pickAndAssignGreedy (x : System.Collections.Generic.KeyValuePair<_,_>) = state {
  let! coloringSoFar = getState
  let n = Seq.choose (Map.tryFind |> flip <| coloringSoFar)  x.Value
  let c = greedyNextColor n
  return! assignColor c x.Key
}
  
let private colorGraphGreedy a = colorGraph pickAndAssignGreedy a

let private addFunctionArgNode args g = 
  let nodeId = -1 |> Node
  let addArg x = (x, nodeId)
  let argNodes = args |> Seq.map addArg 
  argNodes |> Seq.append g

let private colorML args = 
     toGraph
  >> getLiveNodes
  >> addFunctionArgNode args
  >> toLivnessGraph
  >> Map.ofSeq
  >> Map.map (konst List.ofSeq)
  >> colorGraphGreedy
  >> (exec |> flip <| Map.empty)

let private replaceVar (coloring:Map<_,_>) v = coloring.[v] 

let private replaceAtom coloring = function 
  | VarAtom v -> replaceVar coloring v |> VarAtom
  | IntLitAtom i -> IntLitAtom i
  | DataRefAtom s -> DataRefAtom s

let private unifyVars c = 
  mapInstruct 
    (replaceVar c) 
    (replaceVar c |> Option.map)
    (replaceAtom c) 
    id

type UnifiedSignature = CompSignature<Location>

let private unifyVariables (signature : MixedSignature ,ml) = 
  let coloring = colorML signature.args ml
  let newIl = ml |> List.map (unifyVars coloring)
  let newSig = 
    {
      UnifiedSignature.args = List.map (replaceVar coloring) signature.args
      name = signature.name
      returnTy = signature.returnTy
    }

  (newSig, newIl)


type UnifiedModule = CompModule<Location>
let unifyModule (m : MLModule) = 
  {
    UnifiedModule.lits = m.lits
    UnifiedModule.funcs = List.map unifyVariables m.funcs
  }

 
