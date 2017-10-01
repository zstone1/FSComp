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
      int terminate = 0;
      int i = 10;
      while (terminate){
        i = i - 1;
        if(i == 2){
            terminate = 1;
        }
      }
      return i;
  }"
    
    let (exit,stdout) = executeInDir {allocation = AffineGreedy; optimization = true} "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
