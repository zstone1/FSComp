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

    public int foo(string s, int a, int b, int c, int d, int e, int f, int g, int h){
      printf(s,a,b,c,d,e,f,g,h);
      return 0;
    }

    public int main(){
      string s = ""Args %i %i %i %i %i %i %i %i"";
      foo(s,1,2,3,4,5,6,7,8);
      return 0;
    }"
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
