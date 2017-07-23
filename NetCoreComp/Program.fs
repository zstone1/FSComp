// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Assignment
open Unification


[<EntryPoint>]
let main argv =
  try
    let prgm = @"
       public int main(){
         int x = 0;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
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
         |> assignModule
         |> serializeModule
//         |> (fun i -> i.funcInstructions.Head |> snd)
//    do p.ainstructs |> List.iter (printfn "%A")
//    1
    let (exit,stdout) = ParserTests.endToEnd.executeInDir "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
