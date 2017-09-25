// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Unification
open InjectMoves
open MixedLang


[<EntryPoint>]
let main argv =
  try
    let prgm = @" 
  public int main(){
    int y = 2 + other();
    return y;
  }
  public int other() {
    return 5;
  } " 
    globalSettings <- {allocation = AffineGreedy}
    let p = prgm 
         |> parseProgram 
//         |> (fun i -> printfn "%A" i; i)
         |> convertModule
        ||> flattenModule
//         |> (fun i -> printfn "%A" i; i)
         |> toML
//         |> (fun i -> printfn "%A" i; i)
         |> unifyModule
//         |> (fun i -> printfn "%A" i; i)
         |> assignMovesToModules
         |> (fun i -> printfn "%A" i; i)
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
