// Learn more about F# at http://fsharp.org

open FParsec
open Parser
open ASTBuilder
open VariableAssignment
open Assembly


[<EntryPoint>]
let main argv =
  try
    let prgm = @" public int main(){
        int x;
        x = 1;
        if(x)
        {
          int y;
          y = 3;
        };
        int y;
        y = 5;
        int z;
        z = Add(y,y);
        z = Add(z,y);
        return z;
    }"
//    let p = (prgm |> parseProgram |> convertModule |> fst |> assignModule |> serializeModule)
    let p = ""
    do System.IO.File.WriteAllText("FSTestTemp/test1.asm", p)
    use assemble = System.Diagnostics.Process.Start("nasm", "-felf64 \"FSTestTemp/test1.asm\" -o \"FSTestTemp/Foo.o\"")
    do assemble.WaitForExit()
    use link = System.Diagnostics.Process.Start("ld", "FSTestTemp/Foo.o -o FSTestTemp/Foo.out ")
    do link.WaitForExit()
    use proc = System.Diagnostics.Process.Start("./FSTestTemp/Foo.out")
    do proc.WaitForExit()
    let rtn = proc.ExitCode
    do printfn "Return code %i" rtn
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
