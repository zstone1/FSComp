// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Unification
open InjectMoves


[<EntryPoint>]
let main argv =
  try
    let prgm =  @"
    public int main(){
      int x = 1;
      int y = 5;
      return Foo(x,y);
    }

    public int Foo(int i, int j){
      return Bar(j,i);
    }
    
    public int Bar(int i, int j){
      return i - j;
    }
    "
    globalSettings <- {allocation = RegGreedy}
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
