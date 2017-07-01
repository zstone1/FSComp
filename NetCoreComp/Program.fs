// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open Flatten
open Assembly


[<EntryPoint>]
let main argv =
  try
    let prgm = @" public int main(){
        int y;
        y = Add(Add(Add(2,2),2),2);
        y = Add(2,Add(2,Add(2,2)));
        return y;
    }"
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> fst
         |> (List.collect (List.map (sprintf "%A")))
         |> String.concat "\n"
    printfn "%s" p
    1
  with 
  | exn -> printfn "%A" exn; 1
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
