ERLC = erlc
ERL_COMPILE_FLAGS = +debug_info
EBIN_DIR   = .
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(subst .erl,.beam,$(ERL_FILES))

## Create needed folders (if not exist):
$(shell [ -d "$(EBIN_DIR)/" ] || mkdir $(EBIN_DIR)/)


.PHONY: all 

all: $(BEAM_FILES)

%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o $(EBIN_DIR) $<

dialyzer:
	dialyzer --src -r .

run:
	/usr/lib/erlang/bin/erl -noshell -s test extreme_depart_test -s erlang halt

testcase1:
	/usr/lib/erlang/bin/erl -noshell -s testcase1 main eventual 3 -s erlang halt		
