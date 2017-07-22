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

let (|AsList|) = function 
  | Exit -> []
  | Step t -> [t]
  | Branch (t1,t2) -> [t1;t2]

let fold acc seed (AsList l) = List.fold acc seed l

type CompNode = {
  instruction : Instruct
  id : int
  next : Next<int>
}

let (|Arithmetic|_|) = function 
  | CmpI (a,b) | AddI (a,b) | SubI (a,b)  | IMulI (a,b)  -> Some (a,b)
  | _ -> None

///What kind of Next does each instruction use?
let (|Return|StepNext|StepJump|BranchJump|) = function
  | ReturnI _ -> Return
  | JnzI l  -> BranchJump l
  | JmpI l  -> StepJump l
  | CmpI _ | AssignI _ | AddI _ | LabelI _ | IMulI _ | SubI _
  | CallI _ 
      -> StepNext

let computeEdges prgm f x xs = 
  let getLab l = List.find (fun i -> f i = LabelI l) prgm
  match f x, xs with 
  | Return, _ -> Exit
  | StepNext, y::_ -> y |> Step
  | StepJump l, _ -> l |> getLab |> Step
  | BranchJump l, (y::_) -> (y, getLab l) |> Branch
  | _ -> failf "An invalid instruction list was generated. %A, %A" x xs


///Given a list of instructions, produces an adjacency map
///of the corresponding computation graph
let buildComputationGraph l = 
  let prgmWithIds = List.indexed l
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

let getVariable = function 
  | IntLitAtom i -> None
  | DataRefAtom i -> None
  | VarAtom v -> Some v

let getVariable' = getVariable >> Option.toList

let getReadVariables = function 
  | CmpI (a,b) | AddI (a,b) | SubI (a,b)  | IMulI (a,b) | CmpI (a,b)
      -> a :: (b |> getVariable')
  | ReturnI x | AssignI (_,x)
      -> x |> getVariable'
  | JmpI _ | JnzI _ | LabelI _
      -> []
  | CallI (_,_,c) 
      -> c |> List.collect getVariable'
let getWrittenVariables = function 
  | AssignI (x,_) | CallI (Some x,_,_) -> [x]
  | _ -> []

type Neighbors<'a> = {ins : 'a list; outs : 'a list}
let SetIns f s = {s with ins = f s.ins}
let SetOuts f s  = {s with outs = f s.outs}

let addEdge start finish = 
     setIfAbsent start {ins = [] ;outs =[]}
  >> update start (SetOuts (cons finish))
  >> setIfAbsent finish {ins = [] ;outs =[]}
  >> update finish (SetIns (cons start))

let computeAdjacency  = 
  let folder adjacencies i node = 
    match node.next with
    | Exit -> setIfAbsent i {ins = []; outs = []} adjacencies
    | Step t -> addEdge i t adjacencies
    | Branch (t1,t2) -> adjacencies |> addEdge i t1 |> addEdge i t2
  Map.fold folder Map.empty

///Given a node where variable v is read,
///This returns all nodes where v is live
///up to readNode.
let rec trackParents' v (adj:Map<_,_>) (g:Map<_,_>) n = state {
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

let trackParents v adj g n = exec (trackParents' v adj g n) []

let getLiveNodes graph = query {
  let adj = computeAdjacency graph
  for (k,c) in Map.toSeq graph do
  for var in getReadVariables c.instruction do
  for l in  trackParents var adj graph k do
  select (var, l)
}

let livenessNodesToGraph (s:seq<_>) = query {
  for (v1, n1) in s do
  join (v2, n2) in s on (n1 = n2)
  select (v1, v2) 
  distinct into x
//  where (fst x <> snd x)
  groupBy (fst x) into g 
  select (g.Key, Seq.map snd g)
}

let assignColor color node = updateStateU <| addOrUpdate node color (konst color)

let greedyColorGraph nextColor (g:Map<_,_>) =
 let evaled = (Map.map (konst List.ofSeq) g)
 state {
  for x in g do 
    let! s = getState
    let n = Seq.choose (Map.tryFind |> flip <| s)  x.Value
    let c = nextColor n
    do! assignColor c x.Key
}

let pickNext cs = Seq.fold max 0 cs |> (+) 1
  
let colorGraph a = greedyColorGraph pickNext a
    
let colorIL il = il 
              |> buildComputationGraph
              |> getLiveNodes
              |> livenessNodesToGraph
              |> Map.ofSeq
              |> colorGraph
              |> (exec |> flip <| Map.empty)

let unifiedName i = sprintf "_unified_%i" i
let replaceVar (coloring:Map<_,_>) v = 
  let newName = coloring.[v] |> unifiedName
  VarName newName
let replaceAtom coloring = function 
  | VarAtom v -> replaceVar coloring v |> VarAtom
  | x -> x

let replaceOptionVar (coloring:Map<_,_>) v = 
  match Map.tryFind v coloring with
  | Some _ -> coloring.[v] |> unifiedName |> VarName |> Some
  | None -> None

let replaceAstVar (coloring :Map<_,_>) (v:ASTVariable) = 
  let newName = Map.tryFind (VarName v.name) coloring 
             |> Option.map unifiedName
             |> Option.defaultValue v.name
  {v with name = newName}



let mapInstruct f f' g h= function 
  | AddI (a,b) -> AddI (f a, g b)
  | CmpI (a,b) -> CmpI (f a, g b)
  | SubI (a,b) -> SubI (f a, g b)
  | IMulI (a,b) -> IMulI (f a, g b)
  | AssignI (a,b) -> AssignI (f a, g b)
  | JmpI (l) -> JmpI (h l)
  | JnzI (l) -> JnzI (h l)
  | CallI (v,l,args) -> CallI (Option.bind f' v, h l, List.map g args)
  | LabelI (l) -> LabelI (h l)
  | ReturnI (v) -> ReturnI (g v)

let unifyVars c = mapInstruct (replaceVar c) (replaceOptionVar c) (replaceAtom c) id


let unifyVariables (signature, il) = 
  let coloring = colorIL il
  let newIl = il |> List.map (unifyVars coloring)
  let newSig = {signature with args = List.map (replaceAstVar coloring) signature.args}
  (newSig, newIl)


let unifyModule (m : FlattenedModule) = 
  {m with funcs = List.map unifyVariables m.funcs}

  