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
      return Foo(1,2,3);
    }

    public int Foo(int i, int j, int k){
      k = 10;
      return i + j + k;
    }"
    globalSettings <- {allocation = AffineGreedy}
//    let p = prgm 
//         |> parseProgram 
//         |> convertModule
//        ||> flattenModule
//         |> (fun i -> printfn "%A" i; i)
//         |> toML
////         |> (fun i -> printfn "%A" i; i)
//         |> unifyModule
////         |> (fun i -> printfn "%A" i; i)
//         |> assignMovesToModules
//         |> (fun i -> printfn "%A" i; i)
//         |> serializeModule
//         |> (fun i -> i.funcInstructions.Head |> snd)
//    do p.ainstructs |> List.iter (printfn "%A")
//    1
    let (exit,stdout) = EndToEnd.executeInDir "manualTest" prgm
    do printfn "std: %s" stdout
    do printfn "code: %i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
