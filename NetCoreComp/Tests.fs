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
         |> fst
         |> assignModule
         |> serializeModule
    do System.IO.Directory.CreateDirectory("/home/zach/cmp/TestOutput/" + testDir) |> ignore
    do System.IO.File.WriteAllText(testOutputDir + testDir + "/test1.asm", p)
    use assemble = proc "nasm" " -felf64 \"test1.asm\" -o \"Foo.o\""
    use link = proc "ld" "Foo.o -o Foo.out "
    use result = proc "./Foo.out" ""
    result.ExitCode

  let execute = executeInDir TestContext.CurrentContext.Test.Name 

  let check i s = Assert.AreEqual( i, s |> execute)

  [<Test>]
  let simplest () = check 5 @"
       public int main(){
           int y = 5;
           return y;
       }"

  [<Test>]
  let ``simple adding`` () = check 4 @"
       public int main(){
           int y;
           y = 2 + 2;
           return y;
       }" 
  [<Test>]
  let ``simple if`` () = check 2 @"
          public int main(){
          int x = 1;
          if(x)
          {
            return 1;
          }
          return 2;
     }"
  [<Test>]
  let ``simple if skip`` () = check 1 @"
      public int main(){
          int x = 0;
          if(x)
          {
            return 1;
          }
          return 2;
     }"
  [<Test>]
  let ``Add a lot`` () = check 40 @"
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
  let ``NestedAdd`` () = check 12 @"
    public int main(){
         int x = (((((2 + 2) + 2) + 2) + 2) + 2);
         return x;
    }"
  [<Test>]
  let ``NestedAddReverse`` () = check 12 @"
    public int main(){
         int x = (2 + (2 + (2 + (2 + (2 + 2)))));
         return x;
    }"  
  [<Test>]
  let ``add variables`` () = check 17 @"
    public int main(){
         int x = 3;
         x = x + x;
         int y = x + 5;
         y = x + y;
         return y;
    }" 

  [<Test>]
  let ``sub and add`` () = check 2 @"
    public int main() {
        int x = 1 - 2 + 3;
        return x;
    }"

  [<Test>]
  let ``while with terminator`` () =check 2 @" 
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
  let ``call no args`` () = check 5 @" 
    public int main(){
      int y = other();
      return y;
    }
    public int other() {
      return 5;
    } " 
  [<Test>]
  let ``call no args 2`` () = check 7 @" 
    public int main(){
      int y = 2 + other();
      return y;
    }
    public int other() {
      return 5;
    } " 
  [<Test>]
  let ``call one arg`` () = check 7 @"
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
  let ``call one arg three deep`` () = check 23 @"
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
  let ``call with 6 args`` () = check 21 @"
      public int main(){
        let x = 1;
        return x + foo(1,1,2,3,5,8);
      }
      
      public int foo(int a, int b, int c, int d, int e, int f){
        return a + b + c + d + e + f;
      }"