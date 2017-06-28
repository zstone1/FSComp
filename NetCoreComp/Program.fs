// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open VariableAssignment


[<EntryPoint>]
let main argv =
    let prgm = @" public int main(){
        if(1)
        {
          int y;
          y = 3;
        };
        int y;
        y = 4;
        int z;
        z = Add(y,y);
        return z;
    }"
//    let proc = System.Diagnostics.Process.Start("foo")
//    let p = proc.WaitForExit()
//    let rtn = proc.ExitCode
    let p = (prgm |> parseProgram |> convertModule |> fst |> assignModule)
    printfn "%A" p
    1

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
