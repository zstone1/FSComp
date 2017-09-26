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

let replaceVar' (coloring:Map<_,_>) = function
  | MLVarName _ as v -> coloring |> Map.tryFind v
  | (RegVar r) -> Reg r |> Some
  | (IncomingStack i) -> Stack (PreStack i) |> Some
  | OutgoingStack -> Stack PostStack |> Some


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

let greedyNextColor cs = Seq.find (fun c -> not <| Seq.contains c cs)

let private pickAndAssignGreedy colorSet key adjs = state {
  let! coloringSoFar = getState
  let disallowed = adjs |> Seq.choose (Map.tryFind |> flip <| coloringSoFar)
  let c = greedyNextColor disallowed colorSet
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
  | [] -> return! assignColor (greedyNextColor disallowed colors) key
  | x::xs -> return! assignColor x key
}
  
let colorGraphGreedy inter afinities = colorGraph (pickAndAssignGreedy colors) inter

let colorGraphStackOnly inter afinities = colorGraph (pickAndAssignGreedy stackColors) inter

let colorGraphAfineGreedy inter affinites = colorGraph (pickAndAssignAfineGreedy affinites) inter
