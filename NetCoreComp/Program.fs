// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly
open Assignment


[<EntryPoint>]
let main argv =
  try
    let prgm = @" 
      public int main(){
        int x = 1;
        int y = foo(0,0,0,0,0,0,0,10);
        return x+y;
      }
      
      public int foo(int a, int b, int c, int d, int e, int f, int g, int h){
        if(g+1){
          return 0;
        }
        return h;
      }" 
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> fst
         |> assignModule
         |> serializeModule
    do printfn "the progam: %A" p
    let rtn = ParserTests.endToEnd.executeInDir "manualTest" prgm
    do printfn "%i" rtn
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
(*
    try
        let prgm = @" public int main(){
            if(1)
            {
              int y;
              y = 3;
            };
            int y;
            y = 4;
            return y;
        }" 
        let p = prgm |> parseProgram |> convertModule
        do printf "%A" p
        1
    with 
        exn -> printf "%A" exn; 1
        *)
