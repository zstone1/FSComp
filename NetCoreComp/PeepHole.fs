module PeepHole
open ASTBuilder
open FSharp.Collections;
open FSharpx
open Flatten
open ComputationGraph
open FSharpx.State
open Option
open MixedLang
open Unification
open InjectMoves


let private cleanPrefix = function 
  //duplicate moves
  | MovA (x,y):: MovA(x',y'):: ps when x = x' -> [MovA(x',y')], ps

  //NoOps
  | MovA (p,q) :: ps when p = q -> [], ps
  | SubA (_, Imm 0) :: ps | AddA (_, Imm 0) :: ps | IMulA (_,Imm 1) :: ps -> [], ps 

  //dumb jumps
  | JmpA n :: LabelA n' :: ps when n = n' -> [LabelA n'], ps

  //Fall though
  | x::xs -> ([x],xs)
  | [] -> ([],[])

let rec peepHoleOptimizeFunc acc = function 
  | [] -> acc |> List.rev
  | l -> let cleaned, more = cleanPrefix l
         peepHoleOptimizeFunc (cleaned @ acc) more


let peepHoleOptimize (m : AsmModule) = {
    lits = m.lits
    funcs = m.funcs |> List.map (peepHoleOptimizeFunc [])
}
