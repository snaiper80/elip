REPO        ?= elip
DEPS_PLT     = $(REPO)_plt
DEF_PLT_APPS = erts kernel stdlib runtime_tools compiler eunit syntax_tools
PLT_APPS     = $(DEF_PLT_APPS)

REBAR = ./rebar -C rebar.config

.PHONY: all rel deps test build_plt analysis coverage

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

run: compile
	erl -pa ebin -pa deps/*/ebin -s $(REPO)

clean:
	@rm -rf $(DEPS_PLT)
	@rm -rf ./.eunit
	@rm -rf ./.rebar
	@$(REBAR) clean

relclean: clean
	@rm -rf rel/$(REPO)

distclean: relclean
	@$(REBAR) delete-deps
	@rm -rf ./ebin
	@rm -rf ./deps

edoc:
	@$(REBAR) skip_deps=true doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) -v skip_deps=true eunit

coverage:
	@open ./.eunit/index.html

typer:
	@typer --plt $(DEPS_PLT) -r ./src

build_plt:
	@dialyzer --build_plt --output_plt $(DEPS_PLT) --apps $(PLT_APPS)

analysis:
	@dialyzer --src ./src --plts $(DEPS_PLT) -Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_behaviours -Wunderspecs
