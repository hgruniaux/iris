FLAGS = --dump-mir

all:
	dune build

violet:
	dune build violet

debug:
	dune exec _build/default/violet/violet.exe -- test.v --dump-ir --arch x86

test: test_x64
	gcc -O3 -S test.c -o test_c.s
	gcc -g test.s test_c.s
	./a.out

test_x86:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch x86 > test.s

test_x64:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch x86-64 > test.s

test_cpulm:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch cpulm > test.s

.PHONY: all LibIris violet
