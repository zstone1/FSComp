module PeepHole
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open AssignHomes
open FSharpx.State
open Option
open MixedLang
open Unification
open InjectMoves

let cullNoOps = function 
  | MovA (p,q) when p = q -> None
  | SubA (_, Imm 0) | AddA (_, Imm 0) | IMulA (_,Imm 1) -> None
  | Nop -> None
  | x -> Some x

let peepHoleOptimizeFunc asm = List.choose cullNoOps asm

let peepHoleOptimize (m : AsmModule) = {
    lits = m.lits
    funcs = m.funcs |> List.map peepHoleOptimizeFunc
}
