module EndToEnd 
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
open Orchestration


let execute setting s =
  let testName =  TestContext.CurrentContext.Test.Name
  let x = executeInDir setting (sprintf "%s_%A" testName setting.allocation) s
  do TestContext.Error.WriteLine(testName + "_" + chromaticNumSum.ToString())
  x
             
let checkvalForSettings (f: _ -> 'a) (i:'a) s settings = 
  try 
    Assert.AreEqual(i, s |> execute settings |> f, sprintf "%A" settings)
  with 
    | CompilerError e -> Assert.Fail(sprintf " Settings %A: Compilation failed: %s " settings e)

let settingsOpts = [
  for al in [AffineGreedy] do
    for opt in [true] do
      yield {allocation = al; optimization = opt;}
]

let checkval (f:_ -> 'a) (i:'a) s = 
  List.iter (checkvalForSettings f i s) settingsOpts

  
let check = checkval id 
let checkCode = checkval fst
let checkStdOut = checkval snd

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
let ``call no args three deep`` () = checkCode 3 @"
  public int main(){
    foo();
    return 3;
  }

  public int foo(){
    bar();
    return 2;
  }
  
  public int bar(){
    return 1;
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
let ``calling convention 8`` () = checkStdOut "Args 1 2 3 4 5 6 7 8\n" @" 

    public int foo(string s, int a, int b, int c, int d, int e, int f, int g, int h){
      printf(s,a,b,c,d,e,f,g,h);
      return 0;
    }

    public int main(){
      string s = ""Args %i %i %i %i %i %i %i %i"";
      foo(s,1,2,3,4,5,6,7,8);
      return 0;
    }"

[<Test>]
let ``calling conventionVerify 9`` () = checkStdOut "Args 1 2 3 4 5 6 7 8 9\n" @" 
    public int main(){
      string s = ""Args %i %i %i %i %i %i %i %i %i"";
      printf(s,1,2,3,4,5,6,7,8,9);
      return 8;
    }"
[<Test>]
let ``order of parameter eval`` () = checkStdOut "1\n2\n3\n4\n5\n6\n7\n8\n" @"
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
let ``order of operations`` () = checkStdOut "1\n2\n3\n" @"
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
[<Test>]
let ``many strings`` () = checkStdOut "a\nb\nc\nd\n" @"
    public int main(){
      printf(""a"");
      printf(""b"");
      printf(""c"");
      printf(""d"");
      return 0;
    }"

[<Test>]
let ``unused vars`` () = checkCode 4 @"
    public int main(){
      int a = 1;
      int b = 2;
      int c = 3;
      return 2 + b;
    }"

[<Test>]
let ``unused params`` () = checkCode 13 @"
    public int main(){
      return Foo(1,2,3);
    }

    public int Foo(int i, int j, int k){
      k = 10;
      return i + j + k;
    }"
[<Test>]
let ``args never unified`` () = checkCode 1 @"
    public int main(){
      return Foo(1,2);
    }

    public int Foo(int i, int j){
      if(i)
      {
        j = 10;
        return j;
      }
      return i;
    }"
[<Test>]
let ``args never unified 2`` () = checkCode 2 @"
    public int main(){
      return Foo(1,2);
    }

    public int Foo(int k, int j){
      if(j)
      {
        k = 10;
        return k;
      }
      return j;
    }"
[<Test>]
let ``unreferenced param`` () = checkCode 1 @"
    public int main(){
      return 1;
    }

    public int Foo(int i){
      return 2;
    }"
[<Test>]
let ``swapparams`` () = checkCode 4 @"
    public int main(){
      int x = 1;
      int y = 5;
      return Foo(x,y);
    }

    public int Foo(int i, int j){
      return Bar(j,i);
    }
    
    public int Bar(int i, int j){
      return i - j;
    }
    "
[<Test>]
let ``unused add`` () = checkCode 4 @"
    public int main(){
      int y = foo() + 2;
      return 4;
    }

    public int foo(){
      return 10;
    }
"