module Orchestration
open FParsec
open Parser
open NUnit.Framework
open Flatten
open Assembly
open ASTBuilder
open Unification
open Swensen.Unquote
open InjectMoves
open MixedLang
open PeepHole
open PruneDeadCode
open ConstantProp
open FSharpx

let compile settings =
  parseProgram 
  >> convertToAST
  >> (flattenToIL |> uncurry)
  >> if settings.optimization then propogateConstants else id
  //Dead brach pruning is required for variable unification to work. so it must always happen before ML is produced.
  >> pruneDeadBranches 
  >> toML
  >> (unifyVariables settings.allocation)
  >> convertToAssembly
  >> if settings.optimization then peepHoleOptimize else id
  >> serializeModule

let private testRoot = "/home/zach/cmp/TestOutput/"

let runProc dir f args = 
  let proc = new System.Diagnostics.Process()
  proc.StartInfo.FileName <- f
  proc.StartInfo.Arguments <- args
  proc.StartInfo.UseShellExecute <- true
  proc.StartInfo.WorkingDirectory <- testRoot + dir
  do proc.Start() |> ignore
  do proc.WaitForExit()
  proc

let executeInDir settings testDir prgm= 
  let proc = runProc testDir
  let p = compile settings prgm
  let dir = testRoot + testDir
  do System.IO.Directory.CreateDirectory(dir) |> ignore
  do System.IO.File.WriteAllText(testRoot + testDir + "/test1.asm", p)
  use assemble = proc "nasm" " -felf64 \"test1.asm\" -o \"Foo.o\""
  use link = proc "gcc" "Foo.o -o Foo.out "
  use result = proc "./Foo.out > result.txt" ""
  let text = System.IO.File.ReadAllText(dir + "/result.txt")
  (result.ExitCode, text)
