[<AutoOpen>]
module CoreTypes

open FSharpx.State

exception CompilerError of string with 
  override x.ToString() = sprintf "Failed to compile %A" x.Data0

type RegAlloctionType = StackOnly | RegGreedy | AffineGreedy

type Settings = {
  allocation : RegAlloctionType
  optimization : bool
}

let public failComp s = raise (CompilerError s)
let public failf a = Printf.kprintf failComp a

let tryExactlyOne = function 
  | [x] -> x |> Some
  | _ -> None

let (|SplitAt|) i = function 
  | xs when List.length xs < i -> (xs,[])
  | xs -> (List.take i xs, List.skip i xs)

let (|MemberF|_|) f l = 
  match List.tryFindIndex f l with
  | Some i -> (List.take i l, l.[i], List.skip (i+1) l) |> Some
  | None -> None

let (|Member|_|) v l = (|MemberF|_|) ((=) v) l

let suffixes l = 
  let accum (rtn, suffix) x = 
      match suffix with 
      | [] -> raise (Failure "should not be called")
      | _::ys -> (x,ys)::rtn, ys
  List.fold accum ([],l) l |> fst |> List.rev

let separate f l = 
  List.fold (fun (y,n) x -> if f x then (x::y, n) else (y,x::n)) ([],[]) l

let cons x xs = x :: xs

let fst_set f (a,b) = (f a, b)

let snd_set f (a,b) = (a, f b)
let (|Present|_|) v m = 
  match Map.tryFind v m with
  | Some s -> Present s |> Some
  | None -> None
let addOrUpdate k s f = function
  | Present k v as g-> Map.add k (f v) g
  | g -> Map.add k s g

let update k f = function 
  | Present k v as g -> Map.add k (f v) g
  | g -> failf "Expected dictionary %A to have key %A" g k

let setIfAbsent k v = addOrUpdate k v id

let mapValues k = Map.foldBack (fun _ v s -> v :: s ) k []
let allKeys g = g |> Map.toSeq |> Seq.map fst 

let printIter l = Seq.iter (printfn "%A") l

let (|Even|Odd|) i = if i%2 =  0 then Even else Odd

[<Literal>] 
let PlusName = "_+"
[<Literal>]
let MinusName = "_-"
[<Literal>]
let MultName = "_*"
