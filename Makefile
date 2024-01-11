FLAGS = --dump-mir --dump-reg-alloc

all:
	dune build

violet:
	dune build violet

debug:
	dune exec _build/default/violet/violet.exe -- test.v --dump-ir --arch x86

test: violet
	bash ./test/test.sh "dune exec _build/default/violet/violet.exe"

test_x86:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch x86 > test.s

test_x64:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch x86-64 > test.s

test_cpulm:
	dune exec _build/default/violet/violet.exe -- test.v $(FLAGS) --arch cpulm > test.s

.PHONY: all LibIris violet test test_x86 test_x64 test_cpulm
