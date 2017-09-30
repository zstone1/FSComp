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

let private assignColor' color node = assignColor node color

let private colorFixed (g : Map<_,_>) vars = state {
  let splitFixed = List.choose (fst >> function 
    | RegVar _ | IncomingStack _ | OutgoingStack _ as x 
      -> tryReplaceVar Map.empty x |> Option.map (fun i -> (x,i))
    | _ -> None)
  let fixedVars = g |> Map.toList |> splitFixed 
  for (fixedVar, loc) in fixedVars do
    do! loc |> assignColor fixedVar
}

///Retains color for those variables that are in fixed positions
///Otherwise use @pickAndAssignColor
let private colorGraph pickColor (g: Map<_,_>) vars = state {
  do! colorFixed g vars
  
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



let colorGraphGreedy inter afinities = colorGraph (pickAndAssignGreedy colors) inter (inter |> allKeys) |>exec<| Map.empty

let colorGraphStackOnly inter afinities = colorGraph (pickAndAssignGreedy stackColors) inter (inter |> allKeys) |>exec<| Map.empty

//Affine coloring

let private pickNextColorAffine coloringSoFar friends key adjs  = 
  let afinities = friends |> List.choose (tryReplaceVar coloringSoFar)

  let disallowed = adjs |> Seq.choose (Map.tryFind |>flip<| coloringSoFar)
  let candidateFriends = [
    for i in afinities do 
      if disallowed |> Seq.contains i |> not
      then yield i
  ]
  match candidateFriends with
  | [] -> false, key, greedyNextColor disallowed colors
  | x::xs -> true, key, x

let rec colorGraphAffineLoop (inter:Map<_,_>) affinities = state {
  let! (currentColors : Map<_,_>) = getState
  let pickNextColorAttempt (k,inter) =
    let friends = affinities |> Map.tryFind k |> Option.defaultValue []
    pickNextColorAffine currentColors friends k inter
 
  let candidates = inter 
                 |> Map.toList
                 |> List.filter ( fst >> currentColors.ContainsKey >> not)
                 |> List.sortByDescending ( snd >> List.length)
                 |> List.map (pickNextColorAttempt)
  match candidates with 
  | [] -> return ()
  | MemberF fst3 (_,(_, key, color),_) 
    -> do! assignColor key color
       do! colorGraphAffineLoop inter affinities
  | (_, key, color) :: _ 
    -> do! assignColor key color
       do! colorGraphAffineLoop inter affinities

}

let private colorGraphAfineGreedy' inter affinites = state {
  do! colorFixed inter (inter |> allKeys)
  do! colorGraphAffineLoop inter affinites
}

let colorGraphAfineGreedy inter affinities = colorGraphAfineGreedy' inter affinities |> exec <| Map.empty
