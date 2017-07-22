// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Assignment
open ComputationGraph


[<EntryPoint>]
let main argv =
  try
    let prgm = @"
      public int main(){
        printf(""a"");
        printf(""b"");
        printf(""c"");
        printf(""d"");
        return 0;
      }"
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> unifyModule
         |> assignModule
         |> serializeModule
//         |> (fun i -> i.funcInstructions.Head |> snd)
//    do p.ainstructs |> List.iter (printfn "%A")
//    1
    let (exit,stdout) = ParserTests.endToEnd.executeInDir "manualTest" prgm
    do printfn "%s" stdout
    do printfn "%i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
