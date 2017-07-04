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
    let prgm = @" public int main(){
        int x = 5;
        int y = x + 6;
        return y;
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
