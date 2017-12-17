.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack test --fast --test-arguments "-j8 --hide-successes" --file-watch

.PHONY: test

test:
	stack test --fast --test-arguments "-j8"

.PHONY: run

run:
	stack build --fast &&\
	mkdir -p output &&\
	echo "${PROGRAM}" | stack exec nyanpasu -- compile > output/code.asm &&\
	cp wrapper/main.c output/main.c &&\
	nasm -f elf32 -o output/code.o output/code.asm &&\
	clang -g -m32 -o output/program output/main.c output/code.o &&\
	output/program

.PHONY: compile

compile:
	stack build --fast &&\
	echo "${PROGRAM}" | stack exec nyanpasu -- compile

.PHONY: sample

sample:
	make run PROGRAM="Let () \\\"a\\\" (Atom (Num () 10)) (Let () \\\"c\\\" (Let () \\\"b\\\" (PrimOp () (NumOp Inc) (Idn () \\\"a\\\")) (Let () \\\"d\\\" (PrimOp () (NumOp Inc) (Idn () \\\"b\\\")) (PrimOp () (NumOp Inc) (Idn () \\\"b\\\")))) (PrimOp () (NumOp Inc) (Idn () \\\"c\\\")))"

.PHONY: compile_sample

compile_sample:
	make compile PROGRAM="Let () \\\"a\\\" (Atom (Num () 10)) (Let () \\\"c\\\" (Let () \\\"b\\\" (PrimOp () (NumOp Inc) (Idn () \\\"a\\\")) (Let () \\\"d\\\" (PrimOp () (NumOp Inc) (Idn () \\\"b\\\")) (PrimOp () (NumOp Inc) (Idn () \\\"b\\\")))) (PrimOp () (NumOp Inc) (Idn () \\\"c\\\")))"

.PHONY: run_dummy

run_dummy:
	cp wrapper/main.c output/main.c &&\
	nasm -f elf32 -o output/code.o output/code.asm &&\
	clang -g -m32 -o output/program output/main.c output/myfunc.c output/code.o &&\
	output/program

.PHONY: clean

clean:
	stack clean

