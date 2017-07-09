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
        string s = ""sup nerds.\nArguments %i. %i. %i. %i. %i. %i. %i. %i"";
        printf(s,1,2,3,4,5,6,7,8);
        return 8;
      }"
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> assignModule
         |> serializeModule
    do printfn "the progam: %A" p
    let (exit,stdout) = ParserTests.endToEnd.executeInDir "manualTest" prgm
    do printfn "%s" stdout
    do printfn "%i" exit
    1
  with 
  | exn -> printfn "fail with: %A" exn; 1
