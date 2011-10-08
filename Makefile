.PHONY: compile xref eunit clean doc check make deps test

PREFIX:=./
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all: deps compile xref # eunit

# for busy typos
m: all
ma: all
mak: all
make: all

deps:
	@./rebar get-deps
#	@./rebar update-deps
#	@./rebar check-deps

edoc:
	@$(REBAR) doc

test:
#	@rm -rf .eunit
#	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyze

eunit: compile xref
	@$(REBAR) eunit

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

xref:
	@$(REBAR) xref
