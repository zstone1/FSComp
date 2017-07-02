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
    
module endToEnd =  
  open FParsec
  open Parser
  open NUnit.Framework
  open Assignment
  open Flatten
  open Assembly
  open ASTBuilder
  open Swensen.Unquote
 
  let testOutputDir = "/home/zach/cmp/TestOutput"
  let runProc f args = 
    let proc = new System.Diagnostics.Process()
    proc.StartInfo.FileName <- f
    proc.StartInfo.Arguments <- args
    proc.StartInfo.UseShellExecute <- true
    proc.StartInfo.WorkingDirectory <- "/home/zach/cmp/TestOutput"
    do proc.Start() |> ignore
    do proc.WaitForExit()
    proc

  let mutable i =  0
  let execute prgm = 
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> fst
         |> assignModule
         |> serializeModule
    do System.IO.File.WriteAllText(testOutputDir + "/test1.asm", p)
    use assemble = runProc "nasm" " -felf64 \"test1.asm\" -o \"Foo.o\""
    use link = runProc "ld" "Foo.o -o Foo.out "
    use proc = runProc "./Foo.out" ""
    proc.ExitCode

  [<Test>]
  let simplest () = 
    Assert.AreEqual(5,@"public int main(){
           int y;
           y = 5;
           return y;
       }" |> execute)

  [<Test>]
  let ``simple adding`` () = 
    Assert.AreEqual(4,@"public int main(){
           int y;
           y = 2 + 2;
           return y;
       }" |> execute)
  [<Test>]
  let ``simple if`` () = 
   Assert.AreEqual(2,@"public int main(){
          int x;
          x = 1;
          if(x)
          {
            return 1;
          };
          return 2;
     }" |> execute)
  [<Test>]
  let ``simple if skip`` () = 
    Assert.AreEqual(1,@"public int main(){
          int x;
          x = 0;
          if(x)
          {
            return 1;
          };
          return 2;
     }" |> execute)
  [<Test>]
  let ``Add a lot`` () = 
    Assert.AreEqual(40,@"public int main(){
         int x;
         x = 0;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         x = x + 2;
         return x;
    }" |> execute) 
  [<Test>]
  let ``NestedAdd`` () = 
    Assert.AreEqual(12,@"public int main(){
         int x;
         x = (((((2 + 2) + 2) + 2) + 2) + 2);
         return x;
    }" |> execute) 
  [<Test>]
  let ``NestedAddReverse`` () = 
    Assert.AreEqual(12,@"public int main(){
         int x;
         x = (2 + (2 + (2 + (2 + (2 + 2)))));
         return x;
    }" |> execute) 
  [<Test>]
  let ``add variables`` () = 
    Assert.AreEqual(17,@"public int main(){
         int x;
         x = 3;
         x = x + x;
         int y;
         y = x + 5;
         y = x + y;
         return y;
    }" |> execute) 

  [<Test>]
  let ``add self`` () = 
    Assert.AreEqual(2, @"public int main() {
      int x;
      x = x + x;
      return x;
      }" |> execute)
 