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

let sigCompare = parseComparison parseSignature
[<Test>]
let ``parse static func``() =
  let expected = {  
    access = "public"
    isStatic = true
    returnTy = "int"
    args = [("foo","bar")]
    name = "f"
  }  
  expected |> sigCompare "public static int f(foo bar)"

[<Test>]
let ``parse non-static func``() =
  let expected = {  
    access = "public"
    isStatic = false
    returnTy = "int"
    args = [("foo","bar"); ("fiz","buz")]
    name = "f"
  }  
  expected |> sigCompare "public int f(foo bar, fiz buz)"

let funcCompare = parseComparison parseFunction

let public rtn5 = @"public int f(){return 5; }"

[<Test>]
let ``parse function``() = 
  let expected = {
    signature = 
      {  
        access = "public"
        isStatic = false
        returnTy = "int"
        args = []
        name = "f"
      }  
    body = [ReturnStat (IntLit 5)] 
  }
  expected |>  funcCompare rtn5
  
(*
module ASTBuilderTests =
  open ASTBuilder
  open System
 
  [<Test>]
  let ``build simple AST``() = 
    let prgm =  @"public int main(){
        int x;
        x = 2;
        if(x)
        {
            return 1;
        };
        return 0;
    }"
    let p = prgm |> parseProgram |> convertModule
    
    Assert.AreEqual(
      [{ signature = {access = Public;
                     isStatic = false;
                     returnTy = IntTy;
                     name = "main";
                     args = [];};
         body =  [Declaration (Var (IntTy,"x")); Assignment ("x",(IntTy, IntLit 2));
    IfStat ((IntTy, Variable "x"),[ReturnStat (IntTy, IntLit 1)]);
    ReturnStat (IntTy, IntLit 0)] }], p)
    
    //let AST = convertModule parsed
    //printf "%A" AST
 
*) 
 
 
