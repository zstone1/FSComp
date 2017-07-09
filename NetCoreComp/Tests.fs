﻿module ParserTests

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
  
  
  [<Test>] 
  let ``variable name``() = Variable "myVar" =! parseExpr "myVar"
  
  [<Test>]
  let ``empty function``() = Func ("foo",[]) =! parseExpr "foo()"
  
  [<Test>]
  let ``one arg function``() = Func ("foo",[IntLit 123]) =! parseExpr "foo(123)"
  
  [<Test>] 
  let ``nested funtion``() = Func ("foo",[Func("bar",[])]) =! parseExpr "foo(bar())"
  
  let statCompare = parseBit parseStatement
  
  [<Test>]
  let ``parse return``() = ReturnStat (IntLit 5) =! statCompare "return 5;"
  
  [<Test>]
  let ``parse execution``() = Execution (Func ("foo",[])) =! statCompare "foo ();"

  [<Test>] 
  let ``parse if``() = IfStat (Variable "x", [ReturnStat (IntLit 5); Execution (Func ("foo", [Variable "y"]))]) 
                       =! statCompare "if( x ) {return 5;foo(y);}"
  [<Test>]
  let ``parse declaration``() = Declaration ("ty","foo") =! statCompare "ty foo;"
  
  [<Test>] 
  let ``parse assignment``() = Assignment ("foo", Variable "x") =! statCompare "foo = x;"
  
  let sigCompare = parseBit parseSignature
  
  [<Test>]
  let ``parse non-static func``() =
    {  
      access = "public"
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
 
  let testOutputDir = "/home/zach/cmp/TestOutput/"
  let runProc dir f args = 
    let proc = new System.Diagnostics.Process()
    proc.StartInfo.FileName <- f
    proc.StartInfo.Arguments <- args
    proc.StartInfo.UseShellExecute <- true
    proc.StartInfo.WorkingDirectory <- "/home/zach/cmp/TestOutput/" + dir
    do proc.Start() |> ignore
    do proc.WaitForExit()
    proc

  let mutable i =  0
  let executeInDir testDir prgm= 
    let proc = runProc testDir
    let p = prgm 
         |> parseProgram 
         |> convertModule
        ||> flattenModule
         |> assignModule
         |> serializeModule
    let dir = "/home/zach/cmp/TestOutput/" + testDir
    do System.IO.Directory.CreateDirectory(dir) |> ignore
    do System.IO.File.WriteAllText(testOutputDir + testDir + "/test1.asm", p)
    use assemble = proc "nasm" " -felf64 \"test1.asm\" -o \"Foo.o\""
    use link = proc "gcc" "Foo.o -o Foo.out "
    use result = proc "./Foo.out > result.txt" ""
    let text = System.IO.File.ReadAllText(dir + "/result.txt")
    (result.ExitCode, text)

  let execute = executeInDir TestContext.CurrentContext.Test.Name 

  let checkval (f: _ -> 'a) (i:'a) s = 
    try 
      Assert.AreEqual(i, s |> execute |> f)
    with 
      | CompilerError e -> Assert.Fail("Compilation failed: " + e)
    
  let check = checkval id 
  let checkCode = checkval fst
  let checkOut = checkval snd

  [<Test>]
  let simplest () = checkCode 5 @"
       public int main(){
           int y = 5;
           return y;
       }"

  [<Test>]
  let ``simple adding`` () = checkCode 4 @"
       public int main(){
           int y;
           y = 2 + 2;
           return y;
       }" 
  [<Test>]
  let ``simple if`` () = checkCode 2 @"
          public int main(){
          int x = 1;
          if(x)
          {
            return 1;
          }
          return 2;
     }"
  [<Test>]
  let ``simple if skip`` () = checkCode 1 @"
      public int main(){
          int x = 0;
          if(x)
          {
            return 1;
          }
          return 2;
     }"
  [<Test>]
  let ``Add a lot`` () = checkCode 40 @"
     public int main(){
         int x = 0;
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
    }"
  [<Test>]
  let ``NestedAdd`` () = checkCode 12 @"
    public int main(){
         int x = (((((2 + 2) + 2) + 2) + 2) + 2);
         return x;
    }"
  [<Test>]
  let ``NestedAddReverse`` () = checkCode 12 @"
    public int main(){
         int x = (2 + (2 + (2 + (2 + (2 + 2)))));
         return x;
    }"  
  [<Test>]
  let ``add variables`` () = checkCode 17 @"
    public int main(){
         int x = 3;
         x = x + x;
         int y = x + 5;
         y = x + y;
         return y;
    }" 

  [<Test>]
  let ``sub and add`` () = checkCode 2 @"
    public int main() {
        int x = 1 - 2 + 3;
        return x;
    }"

  [<Test>]
  let ``while with terminator`` () =checkCode 2 @" 
    public int main(){
        int terminate = 0;
        int i = 10;
        while (terminate){
          i = i-1;
          if(i - 2){
              terminate = 1;
          }
        }
        return i;
    }"
  [<Test>]
  let ``call no args`` () = checkCode 5 @" 
    public int main(){
      int y = other();
      return y;
    }
    public int other() {
      return 5;
    } " 
  [<Test>]
  let ``call no args 2`` () = checkCode 7 @" 
    public int main(){
      int y = 2 + other();
      return y;
    }
    public int other() {
      return 5;
    } " 
  [<Test>]
  let ``call one arg`` () = checkCode 7 @"
    public int main(){
      int x = 2;
      int y = bar(x);
      return y + x;
    } 
    
    public int bar(int asdf){
      int x = asdf + 3;
      return x;
    }"

  [<Test>]
  let ``call one arg three deep`` () = checkCode 23 @"
    public int main(){
      int x = 2;
      int y = bar(x);
      return y + x;
    } 
    
    public int bar(int asdf){
      int x = asdf + 3;
      return x + baz(x);
    }
    public int baz(int asdf){
      int x = asdf + 11;
      return x;
    }
    "
  [<Test>]
  let ``call with 6 args`` () = checkCode 21 @"
      public int main(){
        int x = 1;
        return x + foo(1,1,2,3,5,8);
      }
      
      public int foo(int a, int b, int c, int d, int e, int f){
        return a + b + c + d + e + f;
      }"
  [<Test>]
  let ``calling convention 8`` () = checkOut "Args 1 2 3 4 5 6 7 8\n" @" 
      public int main(){
        string s = ""Args %i %i %i %i %i %i %i %i"";
        printf(s,1,2,3,4,5,6,7,8);
        return 8;
      }"

  [<Test>]
  let ``calling conventionVerify 9`` () = checkOut "Args 1 2 3 4 5 6 7 8 9\n" @" 
      public int main(){
        string s = ""Args %i %i %i %i %i %i %i %i %i"";
        printf(s,1,2,3,4,5,6,7,8,9);
        return 8;
      }"
  [<Test>]
  let ``order of parameter eval`` () = checkOut "1\n2\n3\n4\n5\n6\n7\n8\n" @"
      public int do(int i){
        printf(""%i"",i);
        return i;
      }
      public int foo(int a, int b, int c, int d, int e, int f, int h, int i){
        return 0;
      }
      public int main(){
        return foo(do(1),do(2),do(3),do(4),do(5),do(6),do(7),do(8));
      }
      "
  [<Test>]
  let ``order of operations`` () = checkOut "1\n2\n3\n" @"
      public int do(int i){
        printf(""%i"",i);
        return i;
      }
      public int main(){
        return do(1) + do(2) + do(3);
      }"

  [<Test>]
  let ``order of operations 2`` () = check (16, "1\n2\n4\n6\n9\n") @"
      public int do(int i){
        printf(""%i"",i);
        return i;
      }
      public int main(){
        return do(1) + do(do(2) + do(4)) + do(9);
      }"