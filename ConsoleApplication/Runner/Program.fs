module runner

let someFSharpFunction (x:string) = x

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
