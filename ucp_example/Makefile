REBAR_PATH=external_tools/rebar

compile:
	rm -rf apps/ucperl && $(REBAR_PATH)/rebar compile && cd apps && ln -s ../../ucperl ./ucperl

clean_test:
	@$(foreach app,$(wildcard apps/*), (rm -rf $(app)/.eunit);)

clean:
	@$(foreach app, ../ucperl  $(wildcard apps/*), (echo "Cleaning $(app) beams"; rm -f $(app)/ebin/* && rm -f $(app)/doc/*.html  $(app)/doc/edoc-info  $(app)/doc/erlang.png  $(app)/doc/stylesheet.css);)

eunit: clean_test compile
	$(REBAR_PATH)/rebar eunit skip_deps=true

generate:
	$(REBAR_PATH)/rebar generate

all: compile clean generate

tools:
	@$(foreach tool,$(wildcard external_tools/*), cd $(tool) &&  make)

get_deps:
	$(REBAR_PATH)/rebar get-deps

plt:
	dialyzer --build_plt -r ./apps

check_plt:
	dialyzer --check_plt

dialyze: compile
	dialyzer -r ./apps