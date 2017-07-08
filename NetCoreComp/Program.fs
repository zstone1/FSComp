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
        return ((-2)*4) + 10 ;
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
