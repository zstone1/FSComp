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
      int x = 1;
      int y = 5;
      return Foo(x,y);
    }

    public int Foo(int i, int j){
      return Bar(j,i);
    }
    
    public int Bar(int i, int j){
      return i - j;
    }
    "
    
    let (exit,stdout) = executeInDir {allocation = AffineGreedy; optimization = true} "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
