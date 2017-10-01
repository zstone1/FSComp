module MixedLang
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open FSharpx.State
open Option

type Register = 
  | RAX
  | RCX
  | RDX
  | RBX
  | RSP
  | RBP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type MixedVar = 
  | MLVarName of string
  | RegVar of Register
  | OutgoingStack //outgoing stack is handled by keeping track of order. A pain, but order matters anyway.
  | IncomingStack of int

type MLAtom = Atom<MixedVar>
type MLInstruct = Instruct<MixedVar>

type MixedSignature = CompSignature<MixedVar>

type StackLoc = 
  | PreStack of int
  | VarStack of int
  | PostStack //PostStack sucks, and is handled by carefully keeping track of the order of operations.

type Location = 
  | Reg of Register
  | Data of string
  | Imm of int
  | Stack of StackLoc 
let toMLVar = function
  | ILVarName s -> MLVarName s

let toMLAtom vMap= function
  | IntLitAtom i -> IntLitAtom i
  | DataRefAtom s -> DataRefAtom s
  | VarAtom v -> VarAtom (vMap v) 
let callingConvention = [RDI; RSI; RDX; RCX; R8; R9]

let getRequirements = function 
  | AddI _  | SubI _ | IMulI _ | SeteI _
  | AssignI _ | JmpI _ | JzI _ | LabelI _
  | JnzI _
    -> []
  | CmpI (v,_)
    -> []
  | ReturnI (v) 
    -> [(v, RegVar RAX)]
  | CallI (v,lab, SplitAt 6 (l,r)) -> 
    [
      yield (v, RegVar RAX)

      yield! callingConvention
          |> List.take l.Length 
          |> List.map (RegVar)
          |> (List.zip l) 
    ]

let incomingArgRequirements (SplitAt 6 (regArgs, stackArgs)) = [
  yield! callingConvention |> List.take regArgs.Length |> List.map RegVar |> List.zip regArgs
  yield! IncomingStack |> List.init stackArgs.Length |> List.zip stackArgs
]

let toMixedSig (sgn :ILSignature) = 
  let assignments = incomingArgRequirements sgn.args
  let newSgn = {
    MixedSignature.name = sgn.name
    MixedSignature.returnTy = sgn.returnTy
    MixedSignature.args = assignments |> List.map snd
  }
  (newSgn, assignments)

let convertFuncToML sgn il = 
  let (newSgn, initAssign) = toMixedSig sgn
  let bodyAssign = il |> List.collect getRequirements
  let requirementsMap = Map.ofList (initAssign @ bodyAssign)
  let convertVar i = requirementsMap.TryFind i |> Option.defaultValue (toMLVar i)
  let newBody = il |> List.map (mapInstructBasic convertVar)

  (newSgn, newBody)
             

type MLModule = CompModule<MixedVar>
let toML (m : FlattenedModule) = 
  let k = 1
  {
    lits = m.lits
    funcs = m.funcs |> List.map (uncurry convertFuncToML)
  }
   



