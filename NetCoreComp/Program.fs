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
  public int main(){
    int x = 2;
    int y = bar(x);
    return y + x;
  } 
  
  public int bar(int asdf){
    int x = asdf + 3;
    return x;
  }"

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
