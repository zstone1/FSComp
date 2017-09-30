module ComputationGraph
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether

type Next<'t> = 
  | Exit
  | Step of 't
  | Branch of 't*'t

let map f = function 
  | Exit -> Exit
  | Step t -> (f t) |> Step
  | Branch (t1,t2) -> (f t1, f t2) |> Branch

let toList = function 
  | Exit -> []
  | Step t -> [t]
  | Branch (t1,t2) -> [t1;t2]

let fold acc seed l = List.fold acc seed (toList l)


type NodeId = Node of int

type CompNode<'v> = {
  instruction : Instruct<'v>
  id : NodeId
  next : Next<NodeId>
}

type Graph<'a, 't> = {
  nodes : Map<NodeId,'a>
  adj : Map<NodeId, 't>
} 

type Neighbors = {ins : NodeId list; outs : NodeId list}

type DiGraph<'a> = Graph<'a,Neighbors>

type CompGraph<'v> = DiGraph<CompNode<'v>>

type StdGraph<'a, 't> when 't :> seq<'a> = Graph<'a,'t>

///What kind of Next does each instruction use?
let private (|Return|StepNext|StepJump|BranchJump|) = function
  | ReturnI _ -> Return
  | JnzI l  -> BranchJump l
  | JmpI l  -> StepJump l
  | CmpI _ | AssignI _ | AddI _ | LabelI _ 
  | IMulI _ | SubI _ | CallI _ 
      -> StepNext

let private computeEdges prgm f x xs = 
  let getLab l = List.find (fun i -> f i = LabelI l) prgm
  match f x, xs with 
  | Return, _ -> Exit
  | StepNext, y::_ -> y |> Step
  | StepJump l, _ -> l |> getLab |> Step
  | BranchJump l, (y::_) -> (y, getLab l) |> Branch
  | _ -> failf "An invalid instruction list was generated. %A, %A" x xs


///Given a list of instructions, produces an adjacency map
///of the corresponding computation graph
let private buildComputationGraph l = 
  let prgmWithIds = l 
                 |> List.indexed
                 |> List.map (fst_set Node)
  let computeNodes (((id,instruct),_) as x) = 
      let node = {
        id = id
        instruction = instruct
        next = x ||> computeEdges prgmWithIds snd |> map fst}
      (id, node)
  prgmWithIds 
  |> suffixes
  |> List.map computeNodes
  |> Map.ofList

let private getVariable = function 
  | IntLitAtom i -> None
  | DataRefAtom i -> None
  | VarAtom v -> Some v

let getReadVariables = function 
  | ReturnI x | AssignI (_, (VarAtom x)) 
    -> [x] 
  | CallI (_,_,c) 
    -> c 
  | CmpI (a, b) | AddI (a,b) | SubI (a,b)  
  | IMulI (a,b) | CmpI (a,b)
    -> match b with 
       | VarAtom b' -> [a; b']
       | IntLitAtom _ | DataRefAtom _ -> [a]
  | JmpI _ | JnzI _ | LabelI _ | AssignI _ 
    -> []

let getWrittenVariables = function 
  | AssignI (x,_) | CallI (x,_,_) -> Some x
  | AddI(v,_) | SubI (v,_) | IMulI (v,_) -> Some v
  | _ -> None

 
let private setIns f s = {s with ins = f s.ins}
let private setOuts f s  = {s with outs = f s.outs}

let private addEdge start finish = 
     setIfAbsent start {ins = [] ;outs =[]}
  >> update start (setOuts (cons finish))
  >> setIfAbsent finish {ins = [] ;outs =[]}
  >> update finish (setIns (cons start))

let private computeAdjacency x = 
  let folder adjacencies i node = 
    match node.next with
    | Exit -> setIfAbsent i {ins = []; outs = []} adjacencies
    | Step t -> addEdge i t adjacencies
    | Branch (t1,t2) -> adjacencies |> addEdge i t1 |> addEdge i t2
  Map.fold folder Map.empty x

let toGraph l = 
  let g = buildComputationGraph l
  let adj = computeAdjacency g
  {
    nodes = g
    adj = adj
  }
 

type LivenessTraversal<'a> = {
  liveVar : 'a
  witnessed : (NodeId * NodeId) list;
  usedAssignments : NodeId List;}
///Traverses up the computation graph, starting at @n, to determine all of the nodes where @v is alive (assuming it is alive at below or in @n) 
let rec private trackParents' (v  ) (compGraph : DiGraph<_>) n = state {
  let! s = getState
  let completedLoop = List.contains n (s.witnessed |> List.map snd)
  let parents = compGraph.adj.[n].ins
  let parentEdges = parents  |> List.map (fun i -> (i,n))
  let writtenVars = compGraph.nodes.[n].instruction |> getWrittenVariables

  match writtenVars with
  | Some v' when v' = v  && s.witnessed.Length > 0
    -> do! (fun st -> { st with usedAssignments = n :: s.usedAssignments}) |> updateStateU
  | Some _ | None 
    -> do! (fun st -> { st with witnessed = parentEdges @ st.witnessed}) |> updateStateU
       if completedLoop
         then return ()
         else do! mapU (trackParents' v compGraph) parents
}        

let trackParents v compGraph n = exec (trackParents' v compGraph n) {witnessed = []; usedAssignments = []; liveVar = v}

///Returns a pair of Variable->int for every node where 
///the variable is live.
let getTraversals compGraph = query {
  for (k,c) in Map.toSeq compGraph.nodes do
  //TODO: optimize away writes to variables that are never read from.
  for var in getReadVariables c.instruction do
  let result = trackParents var compGraph k 
  select result
}