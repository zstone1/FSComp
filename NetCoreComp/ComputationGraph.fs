module ComputationGraph
open Flatten
open ASTBuilder

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
  | CmpI _ | AssignI _ | AddI _ | LabelI _
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

///A graph traversal that requires: For every loop,
///There is exactly one path from the root of the graph
///to the loop (where the path does not have any edge on the loop).
let getLiveNodes v (graph:Map<int,CompNode>) = 
  let result = Map.empty
  let rec traverse (liveBranch, state) key = 
    if Map.containsKey key state 
    then state //becaues of unique paths, no need to duplicate work.
    else
      let {instruction = i; next = n} = graph.[key] 
      let isLive = (i |> getReadVariables |> List.contains v) || liveBranch
      let newState = Map.add key isLive
      fold (fun g1 g2 -> Map.ofList (Map.toList g1 @ Map.toList g2)) state n 

    

  

  
  