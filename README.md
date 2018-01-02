[![Build Status](https://gitlab.com/gilmi/nyanpasu/badges/master/build.svg)](https://gitlab.com/gilmi/nyanpasu/commits/master/)

# nyanpasu

Building a compiler from a simple expression oriented language to x86 by incrementally adding features
and using a [C wrapper](wrapper/main.c). You might enjoy following through the commit history!

Following [cs4410](http://www.ccs.neu.edu/course/cs4410/) and [cs75](https://www.cs.swarthmore.edu/~jpolitz/cs75/s16) in Haskell.

> "Current" Status: Implemented pairs

- Build with `stack`
- [Sample code examples](src/Language/Nyanpasu/Samples.hs)
- Use `stack exec nyanpasu -- sample-programs --escape-hard` to print sample programs in a way that can be eaten by:
- `make run-program PROGRAM=""` <- put your `--escaped-hard` program here

Example:

```sh
➜ make run-program PROGRAM="Program {progDefs = [Fun () \\\"factorial\\\" [\\\"n\\\"] (If () (PrimBinOp () (NumBinOp Eq) (Atom (Num () 0)) (Idn () \\\"n\\\")) (Atom (Num () 1)) (PrimBinOp () (NumBinOp Mul) (Idn () \\\"n\\\") (Call () \\\"factorial\\\" [PrimBinOp () (NumBinOp Sub) (Idn () \\\"n\\\") (Atom (Num () 1))])))], progMain = Call () \\\"factorial\\\" [Atom (Num () 5)]}"
stack build --fast &&\
mkdir -p output &&\
echo "Program {progDefs = [Fun () \"factorial\" [\"n\"] (If () (PrimBinOp () (NumBinOp Eq) (Atom (Num () 0)) (Idn () \"n\")) (Atom (Num () 1)) (PrimBinOp () (NumBinOp Mul) (Idn () \"n\") (Call () \"factorial\" [PrimBinOp () (NumBinOp Sub) (Idn () \"n\") (Atom (Num () 1))])))], progMain = Call () \"factorial\" [Atom (Num () 5)]}" | stack exec nyanpasu -- compile-program > output/code.asm &&\
cp wrapper/main.c output/main.c &&\
nasm -f elf32 -o output/code.o output/code.asm &&\
clang -g -m32 -o output/program output/main.c output/code.o &&\
output/program
[snip]
120
```

Yeah, it's weird and inconvenient, but it works for now.


