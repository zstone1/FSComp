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
        int j = 6;
        int k = j + j;
        return k;
      }"
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> (fun i -> i.funcs)
         |> List.head 
         |> snd
         |> buildComputationGraph
         |> getLiveNodes
    do p |> Seq.iter (printfn "%A")
    1
//    let (exit,stdout) = ParserTests.endToEnd.executeInDir "manualTest" prgm
//    do printfn "%s" stdout
//    do printfn "%i" exit
//    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
