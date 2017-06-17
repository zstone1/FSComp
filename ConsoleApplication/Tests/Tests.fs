module ParserTests
open FParsec
open NUnit.Framework
open NUnit.Framework.Constraints
open Parser

[<Test>]
let ``one plus one`` ()= Assert.IsTrue(1+1=2)
  
let parseComparison parser str expected = 
  match run parser str with
  | Success(result,_,_) -> Assert.AreEqual(expected, result)
  | x -> failwithf "failed to parse %s with %A" str x
  
let exprCompare = parseComparison parseExpression

[<Test>]
let ``parse int literal``() = 
  (123 |> IntLit |>Atom) |> exprCompare "123 " 

let strLitCompare input result = (result |> StringLit |> Atom) |> exprCompare input
[<Test>]
let ``parse string literals``() = "foo" |> strLitCompare "\"foo\"" 
[<Test>]
let ``escape slashes``() = @"a\" |> strLitCompare @"""a\\"""
[<Test>]
let ``escape others``() = "\n\r" |> strLitCompare @"""\n\r"""

[<Test>]
let ``empty function`` = Func (( 

