
APP := mme
VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' apps/$(APP)/$(APP).app.src)

REBAR='./rebar'

.PHONY: deps docs

all: compile

compile: deps 
	$(REBAR) -v compile

app:
	@./rebar compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

clean:
	$(REBAR) clean
	-@rm -rf .eunit/*.beam erl_crash.dump ebin/*.beam
	-@rm -f *_fsm.beam *_fsm.dot *_fsm.erl *_fsm.jpeg
	-@rm -f *_eqc.beam *_eqc.dot *_eqc.erl *_eqc.jpg


distclean:
	$(REBAR) clean delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

##################################################################
SUT ?= tradepost
SUITE=$(SUT)_tests
FSM_DYNAMIC=ebin/fsm_dynamic

ERLANG_PATH=-pa apps/*/ebin -pa deps/*/ebin -pa ebin
ERLANG_INCLUDE=-I include
BINDIR=ebin

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

shell:
	exec erl ${ERLANG_PATH} -boot start_sasl -s mme

eunit:
	$(REBAR) -v eunit skip_deps=true suites=$(SUITE)

eunit-shell:
	exec erl ${ERLANG_PATH} -pa .eunit -boot start_sasl

vztest:
	erlc ${ERLANG_INCLUDE} -DTEST -o ${BINDIR} test/${SUITE}.erl
	erl -noshell ${ERLANG_PATH} -eval "eunit:test(${SUITE}, [verbose])." -run init stop	
	VIEWER=firefox ${FSM_DYNAMIC} test/$(SUT).erl

##################################################################
