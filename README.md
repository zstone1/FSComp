# FSComp
A compiler for C targeting 64 bit Nasm.

Still under development, so definitely not production polish.

To get started reading this code, first consider the file dependency order:

    <compile Include="FSharpx.fs"/> //This is just utility stuff moved into dotnet core land. See https://github.com/fsprojects/FSharpx.Collections
    <compile Include="CoreTypes.fs"/>
    <compile Include="Parser.fs"/>
    <compile Include="ASTBuilder.fs"/>
    <Compile Include="Flatten.fs"/>
    <Compile Include="MixedLang.fs"/>
    <Compile Include="ComputationGraph.fs"/>
    <Compile Include="ConstantProp.fs"/>
    <Compile Include="PruneDeadCode.fs"/>
    <Compile Include="CopyProp.fs"/>
    <Compile Include="GraphColorings.fs"/>
    <Compile Include="Unification.fs"/>
    <Compile Include="InjectMoves.fs"/>
    <Compile Include="PeepHole.fs"/>
    <Compile Include="Assembly.fs"/>
    <Compile Include="Orchestration.fs"/>
    <Compile Include="Tests.fs"/>
    
1) Start by looking at the tests, and it will at least give you a sense of what I'm trying to compile. 
2) Next take a look at the orchestrator. It actually manages the pipeline of the underlying components. It will give you a sense of what steps exist, and how they are related.
3) Next, I would walk through the major type transformations:
  - String -> Abstract Syntax Tree. This is parsing and type checking. See Parser.fs and ASTBuilder.fs
  - Abstract Syntax Tree -> IL. This flattens the tree into a list of instructions with labels and jumps. It also introdces loads of temp variables. See Flatten.fs
  - IL -> ML. ML stands for Mixed Language. As in a mix of assembly and IL. This representation uses IL looking instructions but variables become physical locations (I.E registers and memory). This step unifies all of the temp variables produced above. Unification.fs has the final steps, and you can dig more deeply from there. 
  - ML -> Assembly. This step is about picking the right assembly instructions for each IL one. Mostly this involves picking the right moves, pushes, pops, ect. See InjectMoves.fs
  - Assembly -> string. This step is mostly serialization. Some details about keeping track of stack depth are managed here too. See Assembly.fs
  
4) At this point, you can dig into whatever look most interesting: the graph coloring, opimizations, ect. 

