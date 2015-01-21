
APP := mme
VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' apps/$(APP)/$(APP).app.src)


REBAR=rebar

.PHONY: deps docs

all: compile

compile: deps
	$(REBAR) -v compile

app:
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

clean:
	$(REBAR) clean
	-@rm -rf .eunit/*.beam erl_crash.dump
	-@rm -f *_fsm.beam *_fsm.dot *_fsm.erl *_fsm.jpeg
	-@rm -f *_eqc.beam *_eqc.dot *_eqc.erl *_eqc.jpg


distclean:
	$(REBAR) clean delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

##################################################################
ERLANG_PATH=-pa apps/*/ebin -pa deps/*/ebin -pa ebin
ERLANG_INCLUDE=-I include
BINDIR=ebin

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

shell:
	exec erl ${ERLANG_PATH} -boot start_sasl -s mme

ct_run:
	./rebar -v ct skip_deps=true suites=mme_transport skip_apps=emm,esm,rrc

# ct_run -dir apps/mme/test/ -suite mme_transport_SUITE

##################################################################
eunit:
	$(REBAR) -v eunit skip_deps=true

eunit-shell:
	exec erl ${ERLANG_PATH} -pa .eunit -boot start_sasl

##################################################################
SUT ?= tradepost
SUITE=$(SUT)_tests
FSM_DYNAMIC=scripts/fsm_dynamic


vztest:
	erlc ${ERLANG_INCLUDE} -DTEST -o ${BINDIR} test/${SUITE}.erl
	erl -noshell ${ERLANG_PATH} -eval "eunit:test(${SUITE}, [verbose])." -run init stop
	VIEWER=firefox ${FSM_DYNAMIC} test/$(SUT).erl

##################################################################

setup_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools crypto tools inets ssl webtool public_key observer
# dialyzer --add_to_plt deps/*/ebin

dialyzer:
	dialyzer apps/*/ebin
