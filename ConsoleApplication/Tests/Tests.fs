module ParserTests
open FParsec
open NUnit.Framework
open NUnit.Framework.Constraints
open Parser

let parseComparison parser str expected = 
  match run parser str with
  | Success(result,_,_) -> Assert.AreEqual(expected, result)
  | x -> failwithf "failed to parse %s with %A" str x
  
let exprCompare = parseComparison parseExpression

[<Test>]
let ``parse int literal``() = 
  IntLit 123 |> exprCompare "123 " 

let strLitCompare input result = StringLit result |> exprCompare input

[<Test>]
let ``parse string literals``() = "foo" |> strLitCompare "\"foo\"" 

[<Test>]
let ``escape slashes``() = @"a\" |> strLitCompare @"""a\\"""

[<Test>]
let ``escape others``() = "\n\r" |> strLitCompare @"""\n\r"""

[<Test>] 
let ``variable name``() = Variable "myVar" |> exprCompare "myVar"

[<Test>]
let ``empty function``() = Func ("foo",[]) |> exprCompare "foo()"

[<Test>]
let ``one arg function``() = Func ("foo",[IntLit 123]) |> exprCompare "foo(123)"

[<Test>]
let ``two arg function``() = Func ("foo",[IntLit 123; StringLit "str"; Variable "v"]) 
                             |> exprCompare "foo(123, \"str\", v)"
[<Test>] 
let ``nested funtion``() = Func ("foo",[Func("bar",[])]) |> exprCompare "foo(bar())"

let statCompare = parseComparison parseStatement

[<Test>]
let ``parse return``() = ReturnStat (IntLit 5) |> statCompare "return 5"

[<Test>]
let ``parse execution``() = Execution (Func ("foo",[])) 
                         |> statCompare "foo ()"
[<Test>] 
let ``parse if``() = IfStat (Variable "x",
                       [ReturnStat (IntLit 5);
                        Execution (Func ("foo", [Variable "y"]))])
                  |> statCompare "if( x ) {return 5;foo(y);}"
[<Test>]
let ``parse declaration``() = Declaration ("ty","foo")
                           |> statCompare "ty foo"

[<Test>] 
let ``parse assignment``() = Assignment ("foo", Variable "x")
                          |> statCompare "foo = x"
