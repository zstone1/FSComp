// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Unification
open InjectMoves
open MixedLang
open Orchestration


[<EntryPoint>]
let main argv =
  try
    let prgm = @"
    public int main(){
      return Foo(1,2,3);
    }

    public int Foo(int i, int j, int k){
      k = 10;
      return i + j + k;
    }"
    let (exit,stdout) = executeInDir {allocation = AffineGreedy} "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
