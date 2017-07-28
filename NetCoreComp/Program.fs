// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Assignment
open Unification
open InjectMoves


[<EntryPoint>]
let main argv =
  try
    let prgm = @"
    public int do(int i){
      printf(""%i"",i);
      return i;
    }
    public int foo(int a, int b, int c, int d, int e, int f, int h, int i){
      return 0;
    }
    public int main(){
      return foo(do(1),do(2),do(3),do(4),do(5),do(6),do(7),do(8));
    }
    "

    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> unifyModule
         //|> (fun i -> printfn "%A" i; i)
         |> unifyModule
         |> (fun i -> printfn "%A" i; i)
         |> assignMovesToModules
         |> serializeModule
//         |> (fun i -> i.funcInstructions.Head |> snd)
//    do p.ainstructs |> List.iter (printfn "%A")
//    1
    let (exit,stdout) = EndToEnd.executeInDir "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
