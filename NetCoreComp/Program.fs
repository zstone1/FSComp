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

    public int foo(string s, int a){
      printf(s,a);
      return 0;
    }

    public int main(){
      string s = ""Args %i"";
      foo(s,1);
      return 0;
    }"
    globalSettings <- {allocation = AffineGreedy}
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> toML
//         |> (fun i -> printfn "%A" i; i)
         |> unifyModule
         |> (fun i -> printfn "%A" i; i)
         |> assignMovesToModules
//         |> (fun i -> printfn "%A" i; i)
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
