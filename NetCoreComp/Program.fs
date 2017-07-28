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
      string s = ""Args %i %i %i %i %i %i %i %i"";
      printf(s,1,2,3,4,5,6,7,8);
      return 8;
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
