# The Hywar GPU programming language

Start the compiler in a haskell enviroment by executing

```
$ ghci Compiler.hs
```

## Useful functions

```
doTaal x
```
Compiles a program to an expression tree. x is the id of the program. Extra programs can be defined in Testing.hs.

```
generateAndSaveLibrary program name
```
Will generate a .cpp and .h file in a folder named according to the name parameter. Will copy the utils.cpp and utils.hpp files as well. Include these in a project linked against an OpenCL SDK and you're good to go.

```
showTaal x
```
Starts a graphviz dot instance with a visual representation of the parse tree.

```
doTest
```
Will compile all original test cases (6003-6007) and execute make. TestKernels.cpp will be compiled with the compiled Hywar programs. The OpenCL SDK location should be written in de makefile.

```
showEx x
```
Will show variable example programs in strings. The Hywar example programs are programs in the range 6003-6007 inclusive.

## Credits
Parsing framework written by Jan Kuper (all the files with the fancy formatting).
The rest is written by Bob Rubbens (all the files without the fancy formatting).

