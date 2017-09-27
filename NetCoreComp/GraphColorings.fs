module GraphColorings
open Flatten
open ASTBuilder
open FSharpx.State
open FSharpx
open Aether
open ComputationGraph
open MixedLang


let regColors = (List.map Reg [RDI;RSI;RDX;RCX;R8;R9;R15; R14; R13; R12; RBP; RBX; RAX])
let stackColors = (Seq.initInfinite (Stack << VarStack) )
let colors = Seq.append regColors stackColors

let tryReplaceVar (coloring:Map<_,_>) = function
  | MLVarName _ as v -> coloring |> Map.tryFind v
  | (RegVar r) -> Reg r |> Some
  | (IncomingStack i) -> Stack (PreStack i) |> Some
  | OutgoingStack -> Stack PostStack |> Some


let private assignColor (node : MixedVar) (color) = 
  updateStateU <| addOrUpdate node color (konst color)

///Retains color for those variables that are in fixed positions
///Otherwise use @pickAndAssignColor
let private colorGraph pickColor (g: Map<_,_>) vars = state {
  //color fixed variables first
  let splitFixed = List.choose (fst >> function 
    | RegVar _ | IncomingStack _ | OutgoingStack _ as x 
      -> tryReplaceVar Map.empty x |> Option.map (fun i -> (x,i))
    | _ -> None)
  for (fixedVar, loc) in splitFixed (g |> Map.toList ) do
    do! loc |> assignColor fixedVar
  
  //Now for the remaining variables
  for key in vars do
    let adjs = g.[key]
    match tryReplaceVar Map.empty key with
    | Some x -> return ()
    | None -> let! c = (pickColor key adjs) 
              do! c |> assignColor key
}

let greedyNextColor cs = Seq.find (fun c -> not <| Seq.contains c cs)

let private pickAndAssignGreedy colorSet key adjs = state {
  let! coloringSoFar = getState
  let disallowed = adjs |> Seq.choose (Map.tryFind |> flip <| coloringSoFar)
  let c = greedyNextColor disallowed colorSet
  return c
}

let private pickAndAssignAfineGreedy (afinity:Map<Atom<_>, Atom<_> list>) key adjs  = state {
  let! coloringSoFar = getState
  let friends = afinity.[key |> VarAtom] 
             |> List.collect (fun i -> afinity.[i])
             |> List.collect (fun i -> afinity.[i])
             |> List.distinct
  let afinities = friends
               |> List.choose (function 
                  | VarAtom v -> tryReplaceVar coloringSoFar v
                  | DataRefAtom _ | IntLitAtom _ -> None)

  let disallowed = adjs |> Seq.choose (Map.tryFind |>flip<| coloringSoFar)
  let candidateFriends = [
    for i in afinities do 
      if disallowed |> Seq.contains i |> not
      then yield i
  ]
  match candidateFriends with
  | [] -> return (greedyNextColor disallowed colors) 
  | x::xs -> return x 
}

let colorGraphGreedy inter afinities = colorGraph (pickAndAssignGreedy colors) inter (inter |> allKeys) |>exec<| Map.empty

let colorGraphStackOnly inter afinities = colorGraph (pickAndAssignGreedy stackColors) inter (inter |> allKeys) |>exec<| Map.empty

let colorGraphAfineGreedy inter affinites = colorGraph (pickAndAssignAfineGreedy affinites) inter (inter |> allKeys) |>exec<| Map.empty

let colorGraphAffineBrute inter affinities = 
  let permutedArgs = 
    inter 
    |> allKeys 
    |> Seq.filter (function 
       | MLVarName _ -> true
       | _ -> false)
    |> Seq.toList
    |> permutations
  let 
  

  1