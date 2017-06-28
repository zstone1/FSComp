module ParserTests

open FParsec
open Parser
open NUnit.Framework
open Swensen.Unquote

module parserTests = 
  let parseBit parser str = 
    match run parser str with
    | Success(result,_,_) -> result
    | x -> failwithf "failed to parse %s with %A" str x
    
  let parseExpr = parseBit parseExpression

  [<Test>]
  let ``parse int literal``() = IntLit 123 =! parseBit parseExpression "123 " 
  
  let strLitCompare input result = StringLit result =! parseExpr input
  
  [<Test>]
  let ``parse string literals``() = StringLit "foo" =! parseExpr "\"foo\"" 
  
  [<Test>]
  let ``escape slashes``() = StringLit @"a\" =! parseExpr @"""a\\"""
  
  [<Test>]
  let ``escape others``() = StringLit "\n\r" =! parseExpr @"""\n\r"""
  
  [<Test>] 
  let ``variable name``() = Variable "myVar" =! parseExpr "myVar"
  
  [<Test>]
  let ``empty function``() = Func ("foo",[]) =! parseExpr "foo()"
  
  [<Test>]
  let ``one arg function``() = Func ("foo",[IntLit 123]) =! parseExpr "foo(123)"
  
  [<Test>]
  let ``two arg function``() = Func ("foo",[IntLit 123; StringLit "str"; Variable "v"]) 
                               =! parseExpr "foo(123, \"str\", v)"
  [<Test>] 
  let ``nested funtion``() = Func ("foo",[Func("bar",[])]) =! parseExpr "foo(bar())"
  
  let statCompare = parseBit parseStatement
  
  [<Test>]
  let ``parse return``() = ReturnStat (IntLit 5) =! statCompare "return 5"
  
  [<Test>]
  let ``parse execution``() = Execution (Func ("foo",[])) =! statCompare "foo ()"

  [<Test>] 
  let ``parse if``() = IfStat (Variable "x", [ReturnStat (IntLit 5); Execution (Func ("foo", [Variable "y"]))]) 
                       =! statCompare "if( x ) {return 5;foo(y);}"
  [<Test>]
  let ``parse declaration``() = Declaration ("ty","foo") =! statCompare "ty foo"
  
  [<Test>] 
  let ``parse assignment``() = Assignment ("foo", Variable "x") =! statCompare "foo = x"
  
  let sigCompare = parseBit parseSignature
  [<Test>]
  let ``parse static func``() =
    {  
      access = "public"
      isStatic = true
      returnTy = "int"
      args = [("foo","bar")]
      name = "f"
    }  =! sigCompare "public static int f(foo bar)"
  
  [<Test>]
  let ``parse non-static func``() =
    {  
      access = "public"
      isStatic = false
      returnTy = "int"
      args = [("foo","bar"); ("fiz","buz")]
      name = "f"
    }  =! sigCompare "public int f(foo bar, fiz buz)"
  
  let funcCompare = parseBit parseFunction
  
  let public rtn5 = @"public int f(){return 5; }"
  
  [<Test>]
  let ``parse function``() = 
    test <@ {
             signature = 
               {  
                 access = "public"
                 isStatic = false
                 returnTy = "int"
                 args = []
                 name = "f"
               }  
             body = [ReturnStat (IntLit 5)] 
    } = funcCompare rtn5 @>
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
    test <@ [{ signature = {
                             access = Public;
                             isStatic = false;
                             returnTy = IntTy;
                             name = "main";
                             args = [];
                           };
       body =  [
                 Declaration (Var (IntTy,"x")); 
                 Assignment (Var (IntTy,"x"),(IntTy, IntLit 2));
                 IfStat ((IntTy, Variable "x"),[ReturnStat (IntTy, IntLit 1)]);
                 ReturnStat (IntTy, IntLit 0) 
               ]
     }] = (prgm |> parseProgram |> convertModule) @>
    
  [<Test>]
  let ``if scoping`` () = 
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
    test <@[{ signature = {access = Public;
                   isStatic = false;
                   returnTy = IntTy;
                   name = "main";
                   args = [];};
       body = [
                 IfStat((IntTy, (IntLit 1)), 
                   [
                     Declaration (Var (IntTy, "y"));
                     Assignment ((Var (IntTy,"y")), (IntTy, IntLit 3))
                   ]);
                 Declaration (Var (IntTy, "y"));
                 Assignment (Var (IntTy, "y"), (IntTy, IntLit 4));
                 ReturnStat (IntTy, Variable "y");
              ]
     }] = (prgm |> parseProgram |> convertModule ) @>
  
module VariableAssignmentTests = 
  open ASTBuilder
  open VariableAssignment
  [<Test>]
  let ``literally anything``() = 
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
    test <@ 
            let p = (prgm |> parseProgram |> convertModule |> assignModule)
            (sprintf "%A" p) = "a"
    @>


*)
 
 
