ERLC_FLAGS=-Wall

SOURCES=$(wildcard src/*.erl)

HEADERS=$(wildcard src/*.hrl)

OBJECTS:=$(SOURCES:src/%.erl=ebin/%.beam)

APPNAME=Module 8 - OSPP (1DT096) 2018

.PHONY: doc doc_url skeleton

all: $(OBJECTS) 

ebin/%.beam: src/%.erl
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	rm -f erl_crash.dump
	rm -Rf ebin/*
	rm -Rf src/*.beam
	rm -Rf doc/*.html
	rm -f doc/edoc-info

doc:
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"."' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

doc_url:
	@echo
	@echo "EDoc index page available at file://$(PWD)/doc/index.html"
	@echo

### EUnit ###

# Make a comma separated list:
# http://ftp.gnu.org/old-gnu/Manuals/make-3.79.1/html_chapter/make_8.html

comma:= ,
empty:=
space:= $(empty) $(empty)

OBJECTS_LIST:= $(subst $(space),$(comma),$(OBJECTS:ebin/%.beam=%))

run_trap: ebin/trap.beam
	erl -noshell -pa ebin -eval 'trap:start()' -s init stop

run_bang: ebin/bang.beam ebin/death.beam
	erl -noshell -pa ebin -eval 'bang:start()' -s init stop

test: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [])' -s init stop

testv: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [verbose])' -s init stop

test_%: ebin/%.beam
	erl -noshell -pa ebin -eval "eunit:test($(subst test_,, $@), [])" -s init stop

testv_%: ebin/%.beam
	erl -noshell -pa ebin -eval "eunit:test($(subst testv_,, $@), [verbose])" -s init stop
