.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack test --fast --file-watch

.PHONY: test

test:
	stack test --fast

.PHONY: run

run:
	stack build --fast &&\
	mkdir -p output &&\
	stack exec nyanpasu -- "Let \"a\" (Num 10) (Let \"c\" (Let \"b\" (Inc (Idn \"a\")) (Let \"d\" (Inc (Idn \"b\")) (Inc (Idn \"b\")))) (Inc (Idn \"c\")))" > output/code.asm &&\
	cp wrapper/main.c output/main.c &&\
	nasm -f elf32 -o output/code.o output/code.asm &&\
	clang -g -m32 -o output/program output/main.c output/code.o &&\
	output/program


.PHONY: clean

clean:
	stack clean
