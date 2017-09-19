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
  | StackArg of int
  | IncomingArg of int

type MLAtom = Atom<MixedVar>
type MLInstruct = Instruct<MixedVar>

type CompSignature<'varTy> = {
  returnTy : Ty
  name : string
  args : 'varTy list
}
type MixedSignature = CompSignature<MixedVar>

let toMLVar = function
  | ILVarName s -> MLVarName s

let toMLAtom = function
  | IntLitAtom i -> IntLitAtom i
  | DataRefAtom s -> DataRefAtom s
  | VarAtom (ILVarName s) -> VarAtom (MLVarName s)
let callingConvention = [RDI; RSI; RDX; RCX; R8; R9]

let toMixedInstruct = function 
  | AddI (a,b) -> [AddI ( toMLVar a, toMLAtom b)]
  | CmpI (a,b) -> [CmpI ( toMLVar a, toMLAtom b)]
  | SubI (a,b) -> [SubI ( toMLVar a, toMLAtom b)]
  | IMulI (a,b) -> [IMulI ( toMLVar a, toMLAtom b)]
  | AssignI (a,b) -> [AssignI ( toMLVar a, toMLAtom b)]
  | JmpI (l) -> [JmpI l]
  | JnzI (l) -> [JnzI l]
  | LabelI (l) -> [LabelI l]
  | ReturnI (v) ->
    [
      AssignI (RegVar RAX, toMLAtom v)
      ReturnI (RegVar RAX |> VarAtom)
    ]
  | CallI (v,lab, SplitAt 6 (l,r)) -> 
      let rtnVar = Option.map toMLVar v

      let regArgs = callingConvention
                 |> List.take l.Length 
                 |> List.map (RegVar)
                 |> (List.zip |> flip <| l)

      let stackArgs = r
                   |> List.indexed 
                   |> List.map (fst_set StackArg )

      let allArgs = regArgs @ stackArgs
                 |> List.map (fst >> VarAtom)

      let moves = regArgs @ stackArgs
               |> List.map ( snd_set toMLAtom >> AssignI)

      [PrepareCall stackArgs.Length] 
      @ moves 
      @ [CallI (RegVar RAX |> Some, lab, allArgs)] 
      @ [CompleteCall stackArgs.Length]
      @ (rtnVar |> Option.map (fun v -> AssignI (v, RegVar RAX |> VarAtom)) |> Option.toList)
    | PrepareCall _ | CompleteCall _ -> failComp "IL should not have any prepare or complete calls"

let toMixedSig (sgn : ILSignature) = {
  MixedSignature.name = sgn.name
  MixedSignature.returnTy = sgn.returnTy
  MixedSignature.args = sgn.args |> List.map toMLVar
}


type CompModule<'varTy> = {funcs : (CompSignature<'varTy> * ( Instruct<'varTy> list)) list; lits : (string * string) list}
type MLModule = CompModule<MixedVar>
let toML (m : FlattenedModule) = 
  let k = 1
  let convertFunc (astSig, instructs) = 
    (toMixedSig astSig, instructs |> List.collect toMixedInstruct)
  {
    lits = m.lits
    funcs = m.funcs |> List.map convertFunc
  }
   



