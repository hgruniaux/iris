VIOLET_FLAGS = --dump-ir
VIOLET = dune exec violet/violet.exe

all:
	dune build

violet:
	dune build violet

debug:
	$(VIOLET) -- test.v --dump-ir --arch x86

test:
	bash ./test/test.sh "$(VIOLET)"

test_x86:
	$(VIOLET) -- test.v $(VIOLET_FLAGS) --arch x86 > test.s

test_x64:
	$(VIOLET) -- test.v $(VIOLET_FLAGS) --arch x86-64 > test.s

test_cpulm:
	$(VIOLET) -- test.v $(VIOLET_FLAGS) --arch cpulm > test.s

.PHONY: all LibIris violet test test_x86 test_x64 test_cpulm
