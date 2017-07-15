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

let (|Return|StepNext|StepJump|BranchJump|) = function
  | ReturnI _ -> Return
  | JnzI l  -> BranchJump l
  | JmpI l  -> StepJump l
  | CmpI _ | AssignI _ | AddI _ | LabelI _
  | SubI _ | IMulI _  | CallI _ 
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
let instructGraph l = 
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

 

  